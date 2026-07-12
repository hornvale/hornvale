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

if __name__ == "__main__":
    test_kills_over_age(); test_count_breach_kills_all_and_flags(); print("circuit_breaker: OK")
