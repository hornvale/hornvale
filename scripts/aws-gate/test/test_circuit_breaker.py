import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
from circuit_breaker import instances_to_kill

def inst(id, age): return {"InstanceId": id, "age": age}

def test_kills_over_age():
    ids, breach = instances_to_kill([inst("a", 8000), inst("b", 10)], now_ts=0, max_age_secs=7200, max_count=10)
    assert ids == ["a"] and not breach

def test_count_breach_kills_all_and_flags():
    boxes = [inst(str(i), 10) for i in range(12)]
    ids, breach = instances_to_kill(boxes, now_ts=0, max_age_secs=7200, max_count=10)
    assert breach and set(ids) == {str(i) for i in range(12)}

def test_age_boundary_is_inclusive():
    # A kill switch must fire AT the threshold, not one second late (age >= max).
    ids, breach = instances_to_kill([inst("at", 7200), inst("below", 7199)], now_ts=0, max_age_secs=7200, max_count=10)
    assert ids == ["at"] and not breach, "age==max_age must be killed; age==max_age-1 must survive"

def test_count_boundary_is_strict():
    # Exactly max_count is NOT a breach; max_count+1 is. Guards both off-by-ones.
    at = [inst(str(i), 10) for i in range(10)]
    _, breach_at = instances_to_kill(at, now_ts=0, max_age_secs=7200, max_count=10)
    assert not breach_at, "len==max_count must not breach"
    over = [inst(str(i), 10) for i in range(11)]
    ids_over, breach_over = instances_to_kill(over, now_ts=0, max_age_secs=7200, max_count=10)
    assert breach_over and len(ids_over) == 11, "len==max_count+1 must breach and kill all"

if __name__ == "__main__":
    test_kills_over_age()
    test_count_breach_kills_all_and_flags()
    test_age_boundary_is_inclusive()
    test_count_boundary_is_strict()
    print("circuit_breaker: OK")
