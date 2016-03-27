-module(tree).
-compile(export_all).


empty() -> {node, 'nil'}.

insert(Key, Value, {node, 'nil'}) ->
    {node, {Key, Value, {node, 'nil'}, {node, 'nil'}}};
insert(NewKey, NewValue, {node, {Key, Value, Smaller, Larger}}) when NewKey < Key ->
    {node, {Key, Value, insert(NewKey, NewValue, Smaller), Larger}};
insert(NewKey, NewValue, {node, {Key, Value, Smaller, Larger}}) when NewKey > Key ->
    {node, {Key, Value, Smaller, insert(NewKey, NewValue, Larger)}};
insert(Key, Value, {node, {Key, _, Smaller, Larger}}) ->
    {node, {Key, Value, Smaller, Larger}}.


lookup(_, {node, 'nil'}) ->
    undefined;
lookup(Key, {node, {Key, Value, _, _}}) ->
    {ok, Value};
lookup(Key, {node, {CurrentKey, _, Smaller, _}}) when Key < CurrentKey ->
    lookup(Key, Smaller);
lookup(Key, {node, {_, _, _, Larger}}) ->
    lookup(Key, Larger).
