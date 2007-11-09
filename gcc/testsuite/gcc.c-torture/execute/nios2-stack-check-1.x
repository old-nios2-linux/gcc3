if { ![istarget "*nios2*"] } {
	return 1
}

set additional_flags "-mstack-check"
return 0
