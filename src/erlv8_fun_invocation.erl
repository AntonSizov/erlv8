-module(erlv8_fun_invocation).
-export([new/6, is_construct_call/1, holder/1, this/1, global/1, vm/1]).

-record(erlv8_fun_invocation, {
	icc,
	holder,
	this,
	ref,
	vm,
	ctx
}).

new(ICC, Holder, This, Ref, VM, Ctx) ->
	#erlv8_fun_invocation{
		icc = ICC,
		holder = Holder,
		this = This,
		ref = Ref,
		vm = VM,
		ctx = Ctx
	}.

is_construct_call(#erlv8_fun_invocation{icc = ICC}) ->
	ICC.

holder(#erlv8_fun_invocation{holder = Holder}) ->
	Holder.

this(#erlv8_fun_invocation{this = This}) ->
	This.

global(#erlv8_fun_invocation{vm = VM, ctx = Ctx}) ->
	erlv8_context:global({VM,Ctx}).

vm(#erlv8_fun_invocation{vm = VM}) ->
	VM.
