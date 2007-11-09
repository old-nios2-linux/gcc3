set torture_eval_before_compile {
    global compiler_conditional_xfail_data
    set compiler_conditional_xfail_data {
        "This test fails on all targets when optimizing." \
        { "*-*-*" } \
        { "-O1" } \
        { "" }
    }
}

return 0
