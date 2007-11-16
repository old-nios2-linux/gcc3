set torture_eval_before_execute {
    global compiler_conditional_xfail_data
    set compiler_conditional_xfail_data {
        "This test fails on nios2 when inlining." \
        { "nios2-*-*" } \
        { "-O3" } \
        { "" }
    }
}

return 0
