# wf set check works

    Code
      check_wf_set("no")
    Condition
      Error:
      ! "no" must be a workflow set, not the string "no".

---

    Code
      check_wf_set(data.frame())
    Condition
      Error:
      ! data.frame() must be a workflow set, not a <data.frame> object.

---

    Code
      rank_results("beeEeEEp!")
    Condition
      Error in `rank_results()`:
      ! x must be a workflow set, not the string "beeEeEEp!".

