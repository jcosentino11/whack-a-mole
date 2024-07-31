-module(whackamole_util).

-include("whackamole_constants.hrl").

-export([uuid/0]).

-spec uuid() -> binary().
uuid() ->
    Id = erlang:make_ref(),
    Id1 = erlang:ref_to_list(Id),
    Id2 = erlang:list_to_binary(Id1),
    Id2.
