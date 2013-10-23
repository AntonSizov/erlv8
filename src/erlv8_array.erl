-module(erlv8_array).

-include("erlv8.hrl").

-export([
	vm/1,
	proplist/1, set_value/3, set_value/4, set_hidden_value/3, get_value/2, get_value/3, get_hidden_value/2, get_hidden_value/3,
	internal_field_count/1, get_internal_field/2, set_internal_field/3,
	set_prototype/2, get_prototype/1, set_accessor/3, set_accessor/4, set_accessor/5, set_accessor/6,
	equals/2, strict_equals/2, call/2, call/3
]).

-compile({no_auto_import,[length/1]}).

-export([list/1, object/1, length/1, push/2, unpush/1, unshift/2, delete/2,

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
call(A1, A2) -> erlv8_object:call(A1, A2).
call(A1, A2, A3) -> erlv8_object:call(A1, A2, A3).

%% ===================================================================
%% extend end
%% ===================================================================

list(#erlv8_array{resource = Resource, vm = VM}) ->
    erlv8_vm:enqueue_tick(VM,{list,Resource}).

object(#erlv8_array{resource = Resource, vm = VM}) ->
    erlv8_object:new(Resource,VM).

new(O) ->
    new(O, undefined).

new(O,V) ->
    #erlv8_array{resource = O, vm = V}.

length(Self) ->
    erlang:length(list(Self)). %% TODO: I guess it will be more efficient if we had a NIF for that?

push(Val, Self) ->
    M = Self:object(),
    M:set_value(length(Self),Val).

unpush(Self) ->
    M = Self:object(),
    M:delete(M:length()-1).

unshift(Val, Self) ->
    M = Self:object(),
    L = length(Self),
    lists:foreach(fun (I) ->
                          M:set_value(L-I,M:get_value(L-I-1))
                  end, lists:seq(0,L-1)),
    M:set_value(0,Val).

delete(Index, Self) ->
    M = Self:object(),
    L = length(Self),
    V = M:get_value(Index),
    lists:foreach(fun (I) ->
                          M:set_value(I,M:get_value(I+1))
                  end, lists:seq(Index,L-1)),
    M:set_value(length,L-1),
    V.
