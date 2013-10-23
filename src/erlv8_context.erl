-module(erlv8_context).
-export([get/1,global/1,new/1]).

get(Server) ->
	erlv8_gen_server2:call(Server,context).

new(Server) ->
	erlv8_gen_server2:call(Server,new_context).

global({Server, Resource}) ->
	erlv8_gen_server2:call(Server,{global, Resource}).


