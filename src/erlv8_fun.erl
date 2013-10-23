-module(erlv8_fun).

-include("erlv8.hrl").

-export([
	vm/1,
	proplist/1, set_value/3, set_value/4, set_hidden_value/3, get_value/2, get_value/3, get_hidden_value/2, get_hidden_value/3,
	internal_field_count/1, get_internal_field/2, set_internal_field/3,
	set_prototype/2, get_prototype/1, delete/2, set_accessor/3, set_accessor/4, set_accessor/5, set_accessor/6,
	equals/2, strict_equals/2
]).

-export([call/1,call/2,call/3,instantiate/1, instantiate/2, object/1,

         new/1, new/2]).

%% ===================================================================
%% extend start
%% ===================================================================

vm(A) -> erlv8_object:vm(A).
proplist(A) -> erlv8_object:proplist(A).
set_value(A1, A2, A3) -> erlv8_object:set_value(A1, A2, A3).
set_value(A1, A2, A3, A4) -> erlv8_object:set_value(A1, A2, A3, A4).
set_hidden_value(A1, A2, A3) -> erlv8_object:set_hidden_value(A1, A2, A3).
get_value(A1, A2) -> erlv8_object:get_value(A1, A2).
get_value(A1, A2, A3) -> erlv8_object:get_value(A1, A2, A3).
get_hidden_value(A1, A2) -> erlv8_object:get_hidden_value(A1, A2).
get_hidden_value(A1, A2, A3) -> erlv8_object:get_hidden_value(A1, A2, A3).
internal_field_count(A1) -> erlv8_object:internal_field_count(A1).
get_internal_field(A1, A2) -> erlv8_object:get_internal_field(A1, A2).
set_internal_field(A1, A2, A3) -> erlv8_object:set_internal_field(A1, A2, A3).
set_prototype(A1, A2) -> erlv8_object:set_prototype(A1, A2).
get_prototype(A1) -> erlv8_object:get_prototype(A1).
set_accessor(A1, A2, A3) -> erlv8_object:set_accessor(A1, A2, A3).
set_accessor(A1, A2, A3, A4) -> erlv8_object:set_accessor(A1, A2, A3, A4).
set_accessor(A1, A2, A3, A4, A5) -> erlv8_object:set_accessor(A1, A2, A3, A4, A5).
set_accessor(A1, A2, A3, A4, A5, A6) -> erlv8_object:set_accessor(A1, A2, A3, A4, A5, A6).
equals(A1, A2) -> erlv8_object:equals(A1, A2).
strict_equals(A1, A2) -> erlv8_object:strict_equals(A1, A2).
delete(A1, A2) -> erlv8_object:delete(A1, A2).

%% ===================================================================
%% extend end
%% ===================================================================

call(Self) ->
    call([], Self).

call({erlv8_object, _,_}=T, Self) ->
    call(T,[], Self);

call(Args, #erlv8_fun{resource = Resource, vm = VM}) when is_list(Args) ->
    erlv8_vm:enqueue_tick(VM, {call, Resource, Args}).

call({erlv8_object, _,_}=This, Args, #erlv8_fun{resource = Resource, vm = VM}) when is_list(Args) ->
    erlv8_vm:enqueue_tick(VM, {call, Resource, Args, This}).

instantiate(Self) ->
    instantiate([], Self).

instantiate(Args, #erlv8_fun{resource = Resource, vm = VM}) when is_list(Args) ->
    erlv8_vm:enqueue_tick(VM, {inst, Resource, Args}).

object(#erlv8_fun{resource = Resource, vm = VM}) ->
    {erlv8_object, Resource, VM}.

new(O) ->
    new(O,undefined).

new(O, V) ->
    #erlv8_fun{resource = O, vm = V}.
