# wf set check works

    Code
      check_wf_set("no")
    Condition
      Error:
      ! "no" must be a workflow set, not the string "no".

---

    Code
      check_wf_set(tibble())
    Condition
      Error:
      ! tibble() must be a workflow set, not a <tbl_df> object.

---

    Code
      rank_results("beeEeEEp!")
    Condition
      Error in `rank_results()`:
      ! x must be a workflow set, not the string "beeEeEEp!".

