use crate::native_function::error::Error;
use crate::value::Value;
use crate::virtual_machine::VirtualMachine;

/// Get the time since the program started.
pub fn uptime(vm: &VirtualMachine, _args: &[Value]) -> Result<Value, Error> {
  let time = vm.start_time.elapsed().as_secs_f64();
  let result = Value::Number(time);

  Ok(result)
}
