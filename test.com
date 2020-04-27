#load "basic" // Is this more like #include or like import?
#load "./mycode.com"
#run build(true);


@compile
build: (run :bool) -> void = {
	#compile_time compiler_save_ctarget("build.c");
	call_clang("build.c")

	If run {
		start_process("./build");
	}
}


// := means const
//caps naming convention = const
PI:f64 := 3.14;

// = means normal var
x: i32 = 10;

compile_time_data: i32 = #run generate_data();

generate_data: () -> str = {
	"10"
} // of type () -> str?

action_example: (foo: (x: i32) ) = { // So, in C this would be:
	// void action_example(void (*foo)(int32_t))
	
}

Bar: (x: i32) := {

}
action_example(Bar);
// action_example((x) => echo(x)); // And now we have inconsistent syntax.
// FIX THE INCONSISTENT LAMBDA SYNTAX
action_example(fn(x: i32) { echo(x); }); // Done

@entry
main: () -> i32 := {
 
}


square: (x : i32) -> i32 := {
	x * x
}

foo: (x : i32) := {

}
