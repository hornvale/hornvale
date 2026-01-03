//! Expression compiler: AST → bytecode.
//!
//! Compiles S-expression ASTs into bytecode chunks for VM execution.
//!
//! # Supported Forms
//!
//! ```lisp
//! ;; Literals
//! 42  3.14  "hello"  :keyword  true  false
//!
//! ;; Arithmetic
//! (+ a b)  (- a b)  (* a b)  (/ a b)  (% a b)  (- x)
//!
//! ;; Comparison
//! (= a b)  (!= a b)  (< a b)  (<= a b)  (> a b)  (>= a b)
//!
//! ;; Logic
//! (and a b)  (or a b)  (not x)
//!
//! ;; Conditionals
//! (if condition then-expr else-expr)
//!
//! ;; Let bindings
//! (let ((x 10) (y 20)) (+ x y))
//!
//! ;; World access
//! (get-component ?e :HP)
//! (has-component ?e :HP)
//!
//! ;; World mutations (buffered, applied after execution)
//! (set! entity :Component value)    ; set component value
//! (relate! :Relation from to)       ; add relation
//! (unrelate! :Relation from to)     ; remove relation
//! (destroy entity)                  ; mark entity for destruction
//!
//! ;; World queries
//! (holder entity)                   ; get entity's container (via Contains)
//! (contents container)              ; list contained entities
//! (location entity)                 ; get entity's room (via InRoom)
//! (exits room)                      ; list exit directions from room
//! (exit-target room direction)      ; get exit destination
//!
//! ;; Predicates
//! (in-scope? actor target)          ; is target reachable by actor?
//! (held-by? item holder)            ; is item held by holder?
//! (portable? entity)                ; is entity portable?
//!
//! ;; Graph traversal queries (return lists of entities)
//! (descendants entity :RelationType max-depth)  ; forward transitive closure
//! (ancestors entity :RelationType max-depth)    ; reverse transitive closure
//!
//! ;; Action context accessors
//! (actor)                           ; current action's actor
//! (direct-object)                   ; current action's direct object
//! (indirect-object)                 ; current action's indirect object
//! (room)                            ; current action's room context
//!
//! ;; Effects
//! (say message)                     ; output message
//!
//! ;; List functions
//! (length list)  (first list)  (rest list)  (nth list index)
//! (empty? list)  (cons item list)
//!
//! ;; Stdlib calls
//! (abs x)  (sqrt x)  (max a b)
//! ```

use crate::Value;
use crate::lang::{Atom, SExpr, Span};
use crate::symbol::Symbol;
use crate::vm::{Chunk, ConstIdx, OpCode, Reg};

/// Compiler error.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum CompileError {
    #[error("undefined variable '{0}' at {1}")]
    UndefinedVariable(String, Span),

    #[error("expected {expected} arguments, got {got} at {span}")]
    ArityMismatch {
        expected: usize,
        got: usize,
        span: Span,
    },

    #[error("invalid syntax: {message} at {span}")]
    InvalidSyntax { message: String, span: Span },

    #[error("register overflow: expression too complex")]
    RegisterOverflow,

    #[error("constant pool overflow")]
    ConstantPoolOverflow,
}

/// A local variable binding.
#[derive(Debug, Clone)]
struct Local {
    name: Symbol,
    register: Reg,
    depth: u32,
}

/// Compiles AST to bytecode.
pub struct Compiler {
    chunk: Chunk,
    /// Local variable bindings.
    locals: Vec<Local>,
    /// Current scope depth.
    scope_depth: u32,
    /// Next available register.
    next_register: Reg,
    /// Current source line (for debug info).
    current_line: u32,
}

impl Compiler {
    /// Create a new compiler.
    pub fn new() -> Self {
        Self {
            chunk: Chunk::new(),
            locals: Vec::new(),
            scope_depth: 0,
            next_register: 0,
            current_line: 1,
        }
    }

    /// Compile an expression and return the chunk.
    pub fn compile(expr: &SExpr) -> Result<Chunk, CompileError> {
        let mut compiler = Compiler::new();
        let dst = compiler.alloc_register()?;
        compiler.compile_expr(expr, dst)?;
        compiler
            .chunk
            .emit(OpCode::Return { src: dst }, compiler.current_line);
        Ok(compiler.chunk)
    }

    /// Compile an expression with pre-bound variables.
    ///
    /// Used for pattern matching where variables like `?e` are bound to values.
    pub fn compile_with_bindings(
        expr: &SExpr,
        bindings: &[(Symbol, Value)],
    ) -> Result<Chunk, CompileError> {
        let mut compiler = Compiler::new();

        // Pre-load bindings into registers
        for (name, value) in bindings {
            let reg = compiler.alloc_register()?;
            let idx = compiler.add_constant(value.clone())?;
            compiler.chunk.emit(OpCode::LoadConst { dst: reg, idx }, 1);
            compiler.locals.push(Local {
                name: *name,
                register: reg,
                depth: 0,
            });
        }

        let dst = compiler.alloc_register()?;
        compiler.compile_expr(expr, dst)?;
        compiler
            .chunk
            .emit(OpCode::Return { src: dst }, compiler.current_line);
        Ok(compiler.chunk)
    }

    /// Compile an expression, placing the result in `dst`.
    fn compile_expr(&mut self, expr: &SExpr, dst: Reg) -> Result<(), CompileError> {
        self.current_line = expr.span().line;

        match expr {
            SExpr::Atom(atom, span) => self.compile_atom(atom, *span, dst),
            SExpr::List(items, span) => {
                if items.is_empty() {
                    // Empty list is nil
                    self.chunk.emit(OpCode::LoadNil { dst }, self.current_line);
                    Ok(())
                } else {
                    self.compile_call(items, *span, dst)
                }
            }
        }
    }

    /// Compile an atom.
    fn compile_atom(&mut self, atom: &Atom, _span: Span, dst: Reg) -> Result<(), CompileError> {
        match atom {
            Atom::Int(n) => {
                let idx = self.add_constant(Value::Int(*n))?;
                self.chunk
                    .emit(OpCode::LoadConst { dst, idx }, self.current_line);
            }
            Atom::Float(n) => {
                let idx = self.add_constant(Value::Float(*n))?;
                self.chunk
                    .emit(OpCode::LoadConst { dst, idx }, self.current_line);
            }
            Atom::Bool(b) => {
                self.chunk
                    .emit(OpCode::LoadBool { dst, value: *b }, self.current_line);
            }
            Atom::String(s) => {
                let idx = self.add_constant(Value::string(s.clone()))?;
                self.chunk
                    .emit(OpCode::LoadConst { dst, idx }, self.current_line);
            }
            Atom::Symbol(sym) => {
                // Check if it's a local variable
                if let Some(reg) = self.resolve_local(*sym) {
                    if reg != dst {
                        self.chunk
                            .emit(OpCode::Move { dst, src: reg }, self.current_line);
                    }
                } else {
                    // Unknown symbol - could be a function name, emit as constant
                    let idx = self.add_constant(Value::Symbol(*sym))?;
                    self.chunk
                        .emit(OpCode::LoadConst { dst, idx }, self.current_line);
                }
            }
            Atom::Keyword(sym) => {
                let idx = self.add_constant(Value::Symbol(*sym))?;
                self.chunk
                    .emit(OpCode::LoadConst { dst, idx }, self.current_line);
            }
        }
        Ok(())
    }

    /// Compile a function call or special form.
    fn compile_call(&mut self, items: &[SExpr], span: Span, dst: Reg) -> Result<(), CompileError> {
        let head = &items[0];
        let args = &items[1..];

        // Check for special forms
        if let Some(sym) = head.as_symbol() {
            match sym.as_str().as_str() {
                // Arithmetic
                "+" => {
                    return self.compile_binary_op(
                        OpCode::Add { dst, a: 0, b: 0 },
                        args,
                        dst,
                        span,
                    );
                }
                "-" => {
                    if args.len() == 1 {
                        return self.compile_unary_op(OpCode::Neg { dst, src: 0 }, args, dst, span);
                    }
                    return self.compile_binary_op(
                        OpCode::Sub { dst, a: 0, b: 0 },
                        args,
                        dst,
                        span,
                    );
                }
                "*" => {
                    return self.compile_binary_op(
                        OpCode::Mul { dst, a: 0, b: 0 },
                        args,
                        dst,
                        span,
                    );
                }
                "/" => {
                    return self.compile_binary_op(
                        OpCode::Div { dst, a: 0, b: 0 },
                        args,
                        dst,
                        span,
                    );
                }
                "%" | "mod" => {
                    return self.compile_binary_op(
                        OpCode::Mod { dst, a: 0, b: 0 },
                        args,
                        dst,
                        span,
                    );
                }

                // Comparison
                "=" | "==" => {
                    return self.compile_binary_op(OpCode::Eq { dst, a: 0, b: 0 }, args, dst, span);
                }
                "!=" => {
                    return self.compile_binary_op(OpCode::Ne { dst, a: 0, b: 0 }, args, dst, span);
                }
                "<" => {
                    return self.compile_binary_op(OpCode::Lt { dst, a: 0, b: 0 }, args, dst, span);
                }
                "<=" => {
                    return self.compile_binary_op(OpCode::Le { dst, a: 0, b: 0 }, args, dst, span);
                }
                ">" => {
                    return self.compile_binary_op(OpCode::Gt { dst, a: 0, b: 0 }, args, dst, span);
                }
                ">=" => {
                    return self.compile_binary_op(OpCode::Ge { dst, a: 0, b: 0 }, args, dst, span);
                }

                // Logic
                "not" => {
                    return self.compile_unary_op(OpCode::Not { dst, src: 0 }, args, dst, span);
                }
                "and" => return self.compile_and(args, dst, span),
                "or" => return self.compile_or(args, dst, span),

                // Control flow
                "if" => return self.compile_if(args, dst, span),
                "let" => return self.compile_let(args, dst, span),
                "do" | "begin" | "progn" => return self.compile_do(args, dst, span),

                // World access
                "get-component" => return self.compile_get_component(args, dst, span),
                "has-component" => return self.compile_has_component(args, dst, span),
                "get" => return self.compile_get_component(args, dst, span), // Alias

                // Query operations
                "descendants" => return self.compile_descendants(args, dst, span),
                "ancestors" => return self.compile_ancestors(args, dst, span),

                // Action context accessors (no arguments)
                "actor" => {
                    if !args.is_empty() {
                        return Err(CompileError::ArityMismatch {
                            expected: 0,
                            got: args.len(),
                            span,
                        });
                    }
                    self.chunk
                        .emit(OpCode::GetContextActor { dst }, self.current_line);
                    return Ok(());
                }
                "direct-object" => {
                    if !args.is_empty() {
                        return Err(CompileError::ArityMismatch {
                            expected: 0,
                            got: args.len(),
                            span,
                        });
                    }
                    self.chunk
                        .emit(OpCode::GetContextDirectObject { dst }, self.current_line);
                    return Ok(());
                }
                "indirect-object" => {
                    if !args.is_empty() {
                        return Err(CompileError::ArityMismatch {
                            expected: 0,
                            got: args.len(),
                            span,
                        });
                    }
                    self.chunk
                        .emit(OpCode::GetContextIndirectObject { dst }, self.current_line);
                    return Ok(());
                }
                "room" => {
                    if !args.is_empty() {
                        return Err(CompileError::ArityMismatch {
                            expected: 0,
                            got: args.len(),
                            span,
                        });
                    }
                    self.chunk
                        .emit(OpCode::GetContextRoom { dst }, self.current_line);
                    return Ok(());
                }

                // Effect operations
                "say" => return self.compile_say(args, dst, span),
                "destroy" => return self.compile_destroy(args, dst, span),

                // World mutations
                "set!" => return self.compile_set_component(args, dst, span),
                "relate!" => return self.compile_relate(args, dst, span),
                "unrelate!" => return self.compile_unrelate(args, dst, span),

                // World queries
                "holder" => return self.compile_holder(args, dst, span),
                "contents" => return self.compile_contents(args, dst, span),
                "exits" => return self.compile_exits(args, dst, span),
                "exit-target" => return self.compile_exit_target(args, dst, span),
                "location" => return self.compile_location(args, dst, span),

                // Predicates
                "in-scope?" => return self.compile_in_scope(args, dst, span),
                "held-by?" => return self.compile_held_by(args, dst, span),
                "portable?" => return self.compile_portable(args, dst, span),

                _ => {}
            }
        }

        // Standard function call
        self.compile_function_call(head, args, dst, span)
    }

    /// Compile a binary operator.
    fn compile_binary_op(
        &mut self,
        op_template: OpCode,
        args: &[SExpr],
        dst: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        if args.len() != 2 {
            return Err(CompileError::ArityMismatch {
                expected: 2,
                got: args.len(),
                span,
            });
        }

        let a = self.alloc_register()?;
        let b = self.alloc_register()?;

        self.compile_expr(&args[0], a)?;
        self.compile_expr(&args[1], b)?;

        let op = match op_template {
            OpCode::Add { .. } => OpCode::Add { dst, a, b },
            OpCode::Sub { .. } => OpCode::Sub { dst, a, b },
            OpCode::Mul { .. } => OpCode::Mul { dst, a, b },
            OpCode::Div { .. } => OpCode::Div { dst, a, b },
            OpCode::Mod { .. } => OpCode::Mod { dst, a, b },
            OpCode::Eq { .. } => OpCode::Eq { dst, a, b },
            OpCode::Ne { .. } => OpCode::Ne { dst, a, b },
            OpCode::Lt { .. } => OpCode::Lt { dst, a, b },
            OpCode::Le { .. } => OpCode::Le { dst, a, b },
            OpCode::Gt { .. } => OpCode::Gt { dst, a, b },
            OpCode::Ge { .. } => OpCode::Ge { dst, a, b },
            _ => unreachable!(),
        };

        self.chunk.emit(op, self.current_line);
        self.free_register(b);
        self.free_register(a);
        Ok(())
    }

    /// Compile a unary operator.
    fn compile_unary_op(
        &mut self,
        op_template: OpCode,
        args: &[SExpr],
        dst: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        if args.len() != 1 {
            return Err(CompileError::ArityMismatch {
                expected: 1,
                got: args.len(),
                span,
            });
        }

        let src = self.alloc_register()?;
        self.compile_expr(&args[0], src)?;

        let op = match op_template {
            OpCode::Neg { .. } => OpCode::Neg { dst, src },
            OpCode::Not { .. } => OpCode::Not { dst, src },
            _ => unreachable!(),
        };

        self.chunk.emit(op, self.current_line);
        self.free_register(src);
        Ok(())
    }

    /// Compile `and` with short-circuit evaluation.
    fn compile_and(&mut self, args: &[SExpr], dst: Reg, span: Span) -> Result<(), CompileError> {
        if args.len() != 2 {
            return Err(CompileError::ArityMismatch {
                expected: 2,
                got: args.len(),
                span,
            });
        }

        // Evaluate first arg
        self.compile_expr(&args[0], dst)?;

        // If false, skip second arg
        let jump_idx = self.chunk.emit(
            OpCode::JumpIfNot {
                cond: dst,
                offset: 0,
            },
            self.current_line,
        );

        // Evaluate second arg
        self.compile_expr(&args[1], dst)?;

        // Patch jump
        let offset = (self.chunk.len() - jump_idx - 1) as i16;
        self.chunk.patch_jump(jump_idx, offset);

        Ok(())
    }

    /// Compile `or` with short-circuit evaluation.
    fn compile_or(&mut self, args: &[SExpr], dst: Reg, span: Span) -> Result<(), CompileError> {
        if args.len() != 2 {
            return Err(CompileError::ArityMismatch {
                expected: 2,
                got: args.len(),
                span,
            });
        }

        // Evaluate first arg
        self.compile_expr(&args[0], dst)?;

        // If true, skip second arg
        let jump_idx = self.chunk.emit(
            OpCode::JumpIf {
                cond: dst,
                offset: 0,
            },
            self.current_line,
        );

        // Evaluate second arg
        self.compile_expr(&args[1], dst)?;

        // Patch jump
        let offset = (self.chunk.len() - jump_idx - 1) as i16;
        self.chunk.patch_jump(jump_idx, offset);

        Ok(())
    }

    /// Compile `if` expression.
    fn compile_if(&mut self, args: &[SExpr], dst: Reg, span: Span) -> Result<(), CompileError> {
        if args.len() != 3 {
            return Err(CompileError::ArityMismatch {
                expected: 3,
                got: args.len(),
                span,
            });
        }

        let cond_reg = self.alloc_register()?;

        // Compile condition
        self.compile_expr(&args[0], cond_reg)?;

        // Jump to else if false
        let else_jump = self.chunk.emit(
            OpCode::JumpIfNot {
                cond: cond_reg,
                offset: 0,
            },
            self.current_line,
        );

        // Compile then branch
        self.compile_expr(&args[1], dst)?;

        // Jump over else
        let end_jump = self
            .chunk
            .emit(OpCode::Jump { offset: 0 }, self.current_line);

        // Patch else jump
        let else_offset = (self.chunk.len() - else_jump - 1) as i16;
        self.chunk.patch_jump(else_jump, else_offset);

        // Compile else branch
        self.compile_expr(&args[2], dst)?;

        // Patch end jump
        let end_offset = (self.chunk.len() - end_jump - 1) as i16;
        self.chunk.patch_jump(end_jump, end_offset);

        self.free_register(cond_reg);
        Ok(())
    }

    /// Compile `let` expression.
    fn compile_let(&mut self, args: &[SExpr], dst: Reg, span: Span) -> Result<(), CompileError> {
        if args.len() < 2 {
            return Err(CompileError::InvalidSyntax {
                message: "let requires bindings and body".to_string(),
                span,
            });
        }

        let bindings = &args[0];
        let body = &args[1..];

        // Parse and compile bindings
        let binding_list = bindings
            .as_list()
            .ok_or_else(|| CompileError::InvalidSyntax {
                message: "let bindings must be a list".to_string(),
                span: bindings.span(),
            })?;

        self.begin_scope();

        for binding in binding_list {
            let items = binding
                .as_list()
                .ok_or_else(|| CompileError::InvalidSyntax {
                    message: "each binding must be a list".to_string(),
                    span: binding.span(),
                })?;

            if items.len() != 2 {
                return Err(CompileError::InvalidSyntax {
                    message: "binding must have exactly 2 elements".to_string(),
                    span: binding.span(),
                });
            }

            let name = items[0]
                .as_symbol()
                .ok_or_else(|| CompileError::InvalidSyntax {
                    message: "binding name must be a symbol".to_string(),
                    span: items[0].span(),
                })?;

            let reg = self.alloc_register()?;
            self.compile_expr(&items[1], reg)?;
            self.declare_local(name, reg);
        }

        // Compile body expressions (last one goes to dst)
        for (i, expr) in body.iter().enumerate() {
            if i == body.len() - 1 {
                self.compile_expr(expr, dst)?;
            } else {
                let temp = self.alloc_register()?;
                self.compile_expr(expr, temp)?;
                self.free_register(temp);
            }
        }

        self.end_scope();
        Ok(())
    }

    /// Compile `do`/`begin`/`progn` - evaluate expressions in sequence, return last.
    fn compile_do(&mut self, args: &[SExpr], dst: Reg, _span: Span) -> Result<(), CompileError> {
        if args.is_empty() {
            // Empty do returns nil (false)
            self.chunk
                .emit(OpCode::LoadBool { dst, value: false }, self.current_line);
            return Ok(());
        }

        // Compile all expressions, only the last one uses dst
        for (i, expr) in args.iter().enumerate() {
            if i == args.len() - 1 {
                self.compile_expr(expr, dst)?;
            } else {
                let temp = self.alloc_register()?;
                self.compile_expr(expr, temp)?;
                self.free_register(temp);
            }
        }

        Ok(())
    }

    /// Compile `get-component` / `get`.
    fn compile_get_component(
        &mut self,
        args: &[SExpr],
        dst: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        if args.len() != 2 {
            return Err(CompileError::ArityMismatch {
                expected: 2,
                got: args.len(),
                span,
            });
        }

        let entity_reg = self.alloc_register()?;
        let component_reg = self.alloc_register()?;

        self.compile_expr(&args[0], entity_reg)?;
        self.compile_expr(&args[1], component_reg)?;

        self.chunk.emit(
            OpCode::GetComponent {
                dst,
                entity: entity_reg,
                component: component_reg,
            },
            self.current_line,
        );

        self.free_register(component_reg);
        self.free_register(entity_reg);
        Ok(())
    }

    /// Compile `has-component`.
    fn compile_has_component(
        &mut self,
        args: &[SExpr],
        dst: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        if args.len() != 2 {
            return Err(CompileError::ArityMismatch {
                expected: 2,
                got: args.len(),
                span,
            });
        }

        let entity_reg = self.alloc_register()?;
        let component_reg = self.alloc_register()?;

        self.compile_expr(&args[0], entity_reg)?;
        self.compile_expr(&args[1], component_reg)?;

        self.chunk.emit(
            OpCode::HasComponent {
                dst,
                entity: entity_reg,
                component: component_reg,
            },
            self.current_line,
        );

        self.free_register(component_reg);
        self.free_register(entity_reg);
        Ok(())
    }

    /// Compile `descendants` query.
    ///
    /// Syntax: `(descendants entity :RelationType max-depth)`
    /// Returns a list of all entities reachable via the relation.
    fn compile_descendants(
        &mut self,
        args: &[SExpr],
        dst: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        if args.len() != 3 {
            return Err(CompileError::ArityMismatch {
                expected: 3,
                got: args.len(),
                span,
            });
        }

        let start_reg = self.alloc_register()?;
        let relation_reg = self.alloc_register()?;
        let depth_reg = self.alloc_register()?;

        self.compile_expr(&args[0], start_reg)?;
        self.compile_expr(&args[1], relation_reg)?;
        self.compile_expr(&args[2], depth_reg)?;

        self.chunk.emit(
            OpCode::Descendants {
                dst,
                start: start_reg,
                relation: relation_reg,
                max_depth: depth_reg,
            },
            self.current_line,
        );

        self.free_register(depth_reg);
        self.free_register(relation_reg);
        self.free_register(start_reg);
        Ok(())
    }

    /// Compile `ancestors` query.
    ///
    /// Syntax: `(ancestors entity :RelationType max-depth)`
    /// Returns a list of all entities that relate to this one (reverse traversal).
    fn compile_ancestors(
        &mut self,
        args: &[SExpr],
        dst: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        if args.len() != 3 {
            return Err(CompileError::ArityMismatch {
                expected: 3,
                got: args.len(),
                span,
            });
        }

        let start_reg = self.alloc_register()?;
        let relation_reg = self.alloc_register()?;
        let depth_reg = self.alloc_register()?;

        self.compile_expr(&args[0], start_reg)?;
        self.compile_expr(&args[1], relation_reg)?;
        self.compile_expr(&args[2], depth_reg)?;

        self.chunk.emit(
            OpCode::Ancestors {
                dst,
                start: start_reg,
                relation: relation_reg,
                max_depth: depth_reg,
            },
            self.current_line,
        );

        self.free_register(depth_reg);
        self.free_register(relation_reg);
        self.free_register(start_reg);
        Ok(())
    }

    /// Compile a `say` effect (output a message).
    fn compile_say(&mut self, args: &[SExpr], dst: Reg, span: Span) -> Result<(), CompileError> {
        if args.len() != 1 {
            return Err(CompileError::ArityMismatch {
                expected: 1,
                got: args.len(),
                span,
            });
        }

        // Compile the message argument
        let msg_reg = self.alloc_register()?;
        self.compile_expr(&args[0], msg_reg)?;

        // Emit the Say opcode
        self.chunk
            .emit(OpCode::Say { message: msg_reg }, self.current_line);

        // say returns nil
        self.chunk.emit(OpCode::LoadNil { dst }, self.current_line);

        self.free_register(msg_reg);
        Ok(())
    }

    /// Compile a `destroy` effect (mark entity for deletion).
    fn compile_destroy(
        &mut self,
        args: &[SExpr],
        dst: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        if args.len() != 1 {
            return Err(CompileError::ArityMismatch {
                expected: 1,
                got: args.len(),
                span,
            });
        }

        // Compile the entity argument
        let entity_reg = self.alloc_register()?;
        self.compile_expr(&args[0], entity_reg)?;

        // Emit the Destroy opcode
        self.chunk
            .emit(OpCode::Destroy { entity: entity_reg }, self.current_line);

        // destroy returns nil
        self.chunk.emit(OpCode::LoadNil { dst }, self.current_line);

        self.free_register(entity_reg);
        Ok(())
    }

    // === World Mutation Compilation ===

    /// Compile `set!` (entity, component, value)
    fn compile_set_component(
        &mut self,
        args: &[SExpr],
        dst: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        if args.len() != 3 {
            return Err(CompileError::ArityMismatch {
                expected: 3,
                got: args.len(),
                span,
            });
        }

        let entity_reg = self.alloc_register()?;
        let comp_reg = self.alloc_register()?;
        let value_reg = self.alloc_register()?;

        self.compile_expr(&args[0], entity_reg)?;
        self.compile_expr(&args[1], comp_reg)?;
        self.compile_expr(&args[2], value_reg)?;

        self.chunk.emit(
            OpCode::SetComponent {
                entity: entity_reg,
                component: comp_reg,
                value: value_reg,
            },
            self.current_line,
        );

        // set! returns nil
        self.chunk.emit(OpCode::LoadNil { dst }, self.current_line);

        self.free_register(value_reg);
        self.free_register(comp_reg);
        self.free_register(entity_reg);
        Ok(())
    }

    /// Compile `relate!` (relation, from, to)
    fn compile_relate(&mut self, args: &[SExpr], dst: Reg, span: Span) -> Result<(), CompileError> {
        if args.len() != 3 {
            return Err(CompileError::ArityMismatch {
                expected: 3,
                got: args.len(),
                span,
            });
        }

        let rel_reg = self.alloc_register()?;
        let from_reg = self.alloc_register()?;
        let to_reg = self.alloc_register()?;

        self.compile_expr(&args[0], rel_reg)?;
        self.compile_expr(&args[1], from_reg)?;
        self.compile_expr(&args[2], to_reg)?;

        self.chunk.emit(
            OpCode::Relate {
                relation: rel_reg,
                from: from_reg,
                to: to_reg,
            },
            self.current_line,
        );

        // relate! returns nil
        self.chunk.emit(OpCode::LoadNil { dst }, self.current_line);

        self.free_register(to_reg);
        self.free_register(from_reg);
        self.free_register(rel_reg);
        Ok(())
    }

    /// Compile `unrelate!` (relation, from, to)
    fn compile_unrelate(
        &mut self,
        args: &[SExpr],
        dst: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        if args.len() != 3 {
            return Err(CompileError::ArityMismatch {
                expected: 3,
                got: args.len(),
                span,
            });
        }

        let rel_reg = self.alloc_register()?;
        let from_reg = self.alloc_register()?;
        let to_reg = self.alloc_register()?;

        self.compile_expr(&args[0], rel_reg)?;
        self.compile_expr(&args[1], from_reg)?;
        self.compile_expr(&args[2], to_reg)?;

        self.chunk.emit(
            OpCode::Unrelate {
                relation: rel_reg,
                from: from_reg,
                to: to_reg,
            },
            self.current_line,
        );

        // unrelate! returns nil
        self.chunk.emit(OpCode::LoadNil { dst }, self.current_line);

        self.free_register(to_reg);
        self.free_register(from_reg);
        self.free_register(rel_reg);
        Ok(())
    }

    // === World Query Compilation ===

    /// Compile `holder` (entity) -> holder entity or nil
    fn compile_holder(&mut self, args: &[SExpr], dst: Reg, span: Span) -> Result<(), CompileError> {
        if args.len() != 1 {
            return Err(CompileError::ArityMismatch {
                expected: 1,
                got: args.len(),
                span,
            });
        }

        let entity_reg = self.alloc_register()?;
        self.compile_expr(&args[0], entity_reg)?;

        self.chunk.emit(
            OpCode::GetHolder {
                dst,
                entity: entity_reg,
            },
            self.current_line,
        );

        self.free_register(entity_reg);
        Ok(())
    }

    /// Compile `contents` (container) -> list of contained entities
    fn compile_contents(
        &mut self,
        args: &[SExpr],
        dst: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        if args.len() != 1 {
            return Err(CompileError::ArityMismatch {
                expected: 1,
                got: args.len(),
                span,
            });
        }

        let container_reg = self.alloc_register()?;
        self.compile_expr(&args[0], container_reg)?;

        self.chunk.emit(
            OpCode::GetContents {
                dst,
                container: container_reg,
            },
            self.current_line,
        );

        self.free_register(container_reg);
        Ok(())
    }

    /// Compile `exits` (room) -> list of direction symbols
    fn compile_exits(&mut self, args: &[SExpr], dst: Reg, span: Span) -> Result<(), CompileError> {
        if args.len() != 1 {
            return Err(CompileError::ArityMismatch {
                expected: 1,
                got: args.len(),
                span,
            });
        }

        let room_reg = self.alloc_register()?;
        self.compile_expr(&args[0], room_reg)?;

        self.chunk.emit(
            OpCode::GetExits {
                dst,
                room: room_reg,
            },
            self.current_line,
        );

        self.free_register(room_reg);
        Ok(())
    }

    /// Compile `exit-target` (room, direction) -> target room or nil
    fn compile_exit_target(
        &mut self,
        args: &[SExpr],
        dst: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        if args.len() != 2 {
            return Err(CompileError::ArityMismatch {
                expected: 2,
                got: args.len(),
                span,
            });
        }

        let room_reg = self.alloc_register()?;
        let dir_reg = self.alloc_register()?;

        self.compile_expr(&args[0], room_reg)?;
        self.compile_expr(&args[1], dir_reg)?;

        self.chunk.emit(
            OpCode::GetExitTarget {
                dst,
                room: room_reg,
                direction: dir_reg,
            },
            self.current_line,
        );

        self.free_register(dir_reg);
        self.free_register(room_reg);
        Ok(())
    }

    /// Compile `location` (entity) -> entity's room or nil
    fn compile_location(
        &mut self,
        args: &[SExpr],
        dst: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        if args.len() != 1 {
            return Err(CompileError::ArityMismatch {
                expected: 1,
                got: args.len(),
                span,
            });
        }

        let entity_reg = self.alloc_register()?;
        self.compile_expr(&args[0], entity_reg)?;

        self.chunk.emit(
            OpCode::GetRoom {
                dst,
                entity: entity_reg,
            },
            self.current_line,
        );

        self.free_register(entity_reg);
        Ok(())
    }

    // === Predicate Compilation ===

    /// Compile `in-scope?` (actor, target) -> bool
    fn compile_in_scope(
        &mut self,
        args: &[SExpr],
        dst: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        if args.len() != 2 {
            return Err(CompileError::ArityMismatch {
                expected: 2,
                got: args.len(),
                span,
            });
        }

        let actor_reg = self.alloc_register()?;
        let target_reg = self.alloc_register()?;

        self.compile_expr(&args[0], actor_reg)?;
        self.compile_expr(&args[1], target_reg)?;

        self.chunk.emit(
            OpCode::InScope {
                dst,
                actor: actor_reg,
                target: target_reg,
            },
            self.current_line,
        );

        self.free_register(target_reg);
        self.free_register(actor_reg);
        Ok(())
    }

    /// Compile `held-by?` (item, holder) -> bool
    fn compile_held_by(
        &mut self,
        args: &[SExpr],
        dst: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        if args.len() != 2 {
            return Err(CompileError::ArityMismatch {
                expected: 2,
                got: args.len(),
                span,
            });
        }

        let item_reg = self.alloc_register()?;
        let holder_reg = self.alloc_register()?;

        self.compile_expr(&args[0], item_reg)?;
        self.compile_expr(&args[1], holder_reg)?;

        self.chunk.emit(
            OpCode::IsHeldBy {
                dst,
                item: item_reg,
                holder: holder_reg,
            },
            self.current_line,
        );

        self.free_register(holder_reg);
        self.free_register(item_reg);
        Ok(())
    }

    /// Compile `portable?` (entity) -> bool
    fn compile_portable(
        &mut self,
        args: &[SExpr],
        dst: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        if args.len() != 1 {
            return Err(CompileError::ArityMismatch {
                expected: 1,
                got: args.len(),
                span,
            });
        }

        let entity_reg = self.alloc_register()?;
        self.compile_expr(&args[0], entity_reg)?;

        self.chunk.emit(
            OpCode::IsPortable {
                dst,
                entity: entity_reg,
            },
            self.current_line,
        );

        self.free_register(entity_reg);
        Ok(())
    }

    /// Compile a standard function call.
    fn compile_function_call(
        &mut self,
        func: &SExpr,
        args: &[SExpr],
        dst: Reg,
        _span: Span,
    ) -> Result<(), CompileError> {
        // Get function name
        let func_name = func
            .as_symbol()
            .ok_or_else(|| CompileError::InvalidSyntax {
                message: "function name must be a symbol".to_string(),
                span: func.span(),
            })?;

        let func_idx = self.add_constant(Value::Symbol(func_name))?;

        // Compile arguments into consecutive registers
        let args_start = self.next_register;
        for arg in args {
            let reg = self.alloc_register()?;
            self.compile_expr(arg, reg)?;
        }

        self.chunk.emit(
            OpCode::Call {
                dst,
                func_idx,
                args: args_start,
                nargs: args.len() as u8,
            },
            self.current_line,
        );

        // Free argument registers
        for _ in args {
            self.next_register -= 1;
        }

        Ok(())
    }

    // === Scope management ===

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;

        // Remove locals from this scope
        while let Some(local) = self.locals.last() {
            if local.depth > self.scope_depth {
                self.free_register(local.register);
                self.locals.pop();
            } else {
                break;
            }
        }
    }

    fn declare_local(&mut self, name: Symbol, register: Reg) {
        self.locals.push(Local {
            name,
            register,
            depth: self.scope_depth,
        });
    }

    fn resolve_local(&self, name: Symbol) -> Option<Reg> {
        for local in self.locals.iter().rev() {
            if local.name == name {
                return Some(local.register);
            }
        }
        None
    }

    // === Register allocation ===

    fn alloc_register(&mut self) -> Result<Reg, CompileError> {
        if self.next_register == 255 {
            return Err(CompileError::RegisterOverflow);
        }
        let reg = self.next_register;
        self.next_register += 1;
        Ok(reg)
    }

    fn free_register(&mut self, reg: Reg) {
        // Simple stack-based allocation: only free if it's the top register
        if reg == self.next_register - 1 {
            self.next_register -= 1;
        }
    }

    fn add_constant(&mut self, value: Value) -> Result<ConstIdx, CompileError> {
        if self.chunk.constants_len() >= u16::MAX as usize {
            return Err(CompileError::ConstantPoolOverflow);
        }
        Ok(self.chunk.add_constant(value))
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::World;
    use crate::lang::parse;
    use crate::vm::{StdLib, VM};

    fn eval(source: &str) -> Value {
        let expr = parse(source).unwrap();
        let chunk = Compiler::compile(&expr).unwrap();
        let world = World::new();
        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        vm.run().unwrap()
    }

    #[test]
    fn test_literal_int() {
        assert_eq!(eval("42"), Value::Int(42));
    }

    #[test]
    fn test_literal_float() {
        assert_eq!(eval("3.14"), Value::Float(3.14));
    }

    #[test]
    fn test_literal_string() {
        assert_eq!(eval(r#""hello""#), Value::string("hello"));
    }

    #[test]
    fn test_literal_bool() {
        assert_eq!(eval("true"), Value::Bool(true));
        assert_eq!(eval("false"), Value::Bool(false));
    }

    #[test]
    fn test_add() {
        assert_eq!(eval("(+ 1 2)"), Value::Int(3));
    }

    #[test]
    fn test_sub() {
        assert_eq!(eval("(- 10 3)"), Value::Int(7));
    }

    #[test]
    fn test_mul() {
        assert_eq!(eval("(* 6 7)"), Value::Int(42));
    }

    #[test]
    fn test_div() {
        assert_eq!(eval("(/ 20 4)"), Value::Int(5));
    }

    #[test]
    fn test_neg() {
        assert_eq!(eval("(- 42)"), Value::Int(-42));
    }

    #[test]
    fn test_nested_arithmetic() {
        assert_eq!(eval("(+ (* 2 3) 4)"), Value::Int(10));
        assert_eq!(eval("(* (+ 1 2) (- 5 2))"), Value::Int(9));
    }

    #[test]
    fn test_comparison_eq() {
        assert_eq!(eval("(= 5 5)"), Value::Bool(true));
        assert_eq!(eval("(= 5 3)"), Value::Bool(false));
    }

    #[test]
    fn test_comparison_lt() {
        assert_eq!(eval("(< 3 5)"), Value::Bool(true));
        assert_eq!(eval("(< 5 3)"), Value::Bool(false));
    }

    #[test]
    fn test_comparison_gt() {
        assert_eq!(eval("(> 5 3)"), Value::Bool(true));
        assert_eq!(eval("(> 3 5)"), Value::Bool(false));
    }

    #[test]
    fn test_not() {
        assert_eq!(eval("(not true)"), Value::Bool(false));
        assert_eq!(eval("(not false)"), Value::Bool(true));
    }

    #[test]
    fn test_and() {
        assert_eq!(eval("(and true true)"), Value::Bool(true));
        assert_eq!(eval("(and true false)"), Value::Bool(false));
        assert_eq!(eval("(and false true)"), Value::Bool(false));
    }

    #[test]
    fn test_or() {
        assert_eq!(eval("(or true false)"), Value::Bool(true));
        assert_eq!(eval("(or false true)"), Value::Bool(true));
        assert_eq!(eval("(or false false)"), Value::Bool(false));
    }

    #[test]
    fn test_if_true() {
        assert_eq!(eval("(if true 1 2)"), Value::Int(1));
    }

    #[test]
    fn test_if_false() {
        assert_eq!(eval("(if false 1 2)"), Value::Int(2));
    }

    #[test]
    fn test_if_condition() {
        assert_eq!(eval("(if (> 5 3) 100 200)"), Value::Int(100));
        assert_eq!(eval("(if (< 5 3) 100 200)"), Value::Int(200));
    }

    #[test]
    fn test_let_simple() {
        assert_eq!(eval("(let ((x 10)) x)"), Value::Int(10));
    }

    #[test]
    fn test_let_multiple() {
        assert_eq!(eval("(let ((x 10) (y 20)) (+ x y))"), Value::Int(30));
    }

    #[test]
    fn test_let_shadowing() {
        assert_eq!(eval("(let ((x 1)) (let ((x 2)) x))"), Value::Int(2));
    }

    #[test]
    fn test_let_nested_reference() {
        assert_eq!(eval("(let ((x 10)) (let ((y (+ x 5))) y))"), Value::Int(15));
    }

    #[test]
    fn test_stdlib_abs() {
        assert_eq!(eval("(abs -5)"), Value::Int(5));
        assert_eq!(eval("(abs 5)"), Value::Int(5));
    }

    #[test]
    fn test_stdlib_max() {
        assert_eq!(eval("(max 3 7)"), Value::Int(7));
    }

    #[test]
    fn test_stdlib_sqrt() {
        assert_eq!(eval("(sqrt 4)"), Value::Float(2.0));
    }

    #[test]
    fn test_get_component() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        let expr = parse("(get-component entity :HP)").unwrap();
        let bindings = [(Symbol::new("entity"), Value::EntityRef(entity))];
        let chunk = Compiler::compile_with_bindings(&expr, &bindings).unwrap();

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        assert_eq!(vm.run().unwrap(), Value::Int(100));
    }

    #[test]
    fn test_has_component() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        let expr = parse("(has-component entity :HP)").unwrap();
        let bindings = [(Symbol::new("entity"), Value::EntityRef(entity))];
        let chunk = Compiler::compile_with_bindings(&expr, &bindings).unwrap();

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        assert_eq!(vm.run().unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_complex_expression() {
        // (if (> (+ 2 3) 4) (abs -10) 0)
        assert_eq!(eval("(if (> (+ 2 3) 4) (abs -10) 0)"), Value::Int(10));
    }

    #[test]
    fn test_error_undefined_variable() {
        let expr = parse("undefined_var").unwrap();
        let chunk = Compiler::compile(&expr);
        // The variable will be compiled as a symbol constant, not an error
        // Only when evaluated will we see it's not a valid call
        assert!(chunk.is_ok()); // Compiles fine, just loads as symbol
    }

    #[test]
    fn test_error_arity_mismatch() {
        let expr = parse("(+ 1 2 3)").unwrap();
        let result = Compiler::compile(&expr);
        assert!(matches!(result, Err(CompileError::ArityMismatch { .. })));
    }

    #[test]
    fn test_short_circuit_and() {
        // (and false (/ 1 0)) should not divide by zero
        assert_eq!(eval("(and false (/ 1 0))"), Value::Bool(false));
    }

    #[test]
    fn test_short_circuit_or() {
        // (or true (/ 1 0)) should not divide by zero
        assert_eq!(eval("(or true (/ 1 0))"), Value::Bool(true));
    }

    #[test]
    fn test_descendants_query() {
        use crate::core::{Cardinality, RelationSchema};

        let mut world = World::new();
        world.register_relation(RelationSchema::new(
            "Contains",
            Cardinality::One,
            Cardinality::Many,
        ));

        // Build: room -> chest -> key
        let room = world.create_entity();
        let chest = world.create_entity();
        let key = world.create_entity();

        world.add_relation("Contains", room, chest);
        world.add_relation("Contains", chest, key);

        let expr = parse("(descendants room :Contains 10)").unwrap();
        let bindings = [(Symbol::new("room"), Value::EntityRef(room))];
        let chunk = Compiler::compile_with_bindings(&expr, &bindings).unwrap();

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        // Should return a list containing chest and key
        let items = result.as_list().expect("expected list");
        assert_eq!(items.len(), 2);
        assert!(items.contains(&Value::EntityRef(chest)));
        assert!(items.contains(&Value::EntityRef(key)));
    }

    #[test]
    fn test_ancestors_query() {
        use crate::core::{Cardinality, RelationSchema};

        let mut world = World::new();
        world.register_relation(RelationSchema::new(
            "Contains",
            Cardinality::One,
            Cardinality::Many,
        ));

        // Build: room -> chest -> key
        let room = world.create_entity();
        let chest = world.create_entity();
        let key = world.create_entity();

        world.add_relation("Contains", room, chest);
        world.add_relation("Contains", chest, key);

        let expr = parse("(ancestors key :Contains 10)").unwrap();
        let bindings = [(Symbol::new("key"), Value::EntityRef(key))];
        let chunk = Compiler::compile_with_bindings(&expr, &bindings).unwrap();

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        // Should return a list containing chest and room (in BFS order)
        let items = result.as_list().expect("expected list");
        assert_eq!(items.len(), 2);
        assert_eq!(items[0], Value::EntityRef(chest)); // depth 1
        assert_eq!(items[1], Value::EntityRef(room)); // depth 2
    }

    #[test]
    fn test_descendants_with_list_functions() {
        use crate::core::{Cardinality, RelationSchema};

        let mut world = World::new();
        world.register_relation(RelationSchema::new(
            "Contains",
            Cardinality::One,
            Cardinality::Many,
        ));

        // Build: room -> item1, item2
        let room = world.create_entity();
        let item1 = world.create_entity();
        let item2 = world.create_entity();

        world.add_relation("Contains", room, item1);
        world.add_relation("Contains", room, item2);

        let expr = parse("(length (descendants room :Contains 10))").unwrap();
        let bindings = [(Symbol::new("room"), Value::EntityRef(room))];
        let chunk = Compiler::compile_with_bindings(&expr, &bindings).unwrap();

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::Int(2));
    }
}
