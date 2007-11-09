# the argument is never used, but the return address is attempted to
# be loaded, but it does not do it successfully and gets an illegal 
# address load
set torture_eval_before_execute {
    global compiler_conditional_xfail_data
    set compiler_conditional_xfail_data {
        "This test fails on nios2 because __builtin_return_address fails." \
        { "nios2-*-*" } \
        { "-O*" } \
        { "-O0" }
    }
}


set additional_flags "-finstrument-functions"
return 0
