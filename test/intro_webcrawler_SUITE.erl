-module(intro_webcrawler_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, all/0]).
-export([add_words/1, urls_by_word/1]).
all() -> [add_words, urls_by_word].
 
init_per_suite(Config) ->
Priv = ?config(priv_dir, Config),
application:set_env(mnesia, dir, Priv),
intro_webcrawler_app:install([node()]),
application:start(mnesia),
application:start(mafiapp),
Config.
 
end_per_suite(_Config) ->
application:stop(mnesia),
ok.

init_per_testcase(_, Config) ->
Config.

end_per_testcase(_, _Config) ->
ok.

urls_by_word(_Config) ->
    ok = intro_webcrawler_app:add_word("s", "search", "http://www.google.com"),
    {"search", _URL} = intro_webcrawler_app:lookup_word("search"),
    {"character","http://fake.url"} = intro_webcrawler_app:lookup_word("character"),
    undefined = intro_webcrawler_app:lookup_word(make_ref()).

add_words(_Config) ->
    ok = intro_webcrawler_app:add_word("c",
    "character",
    "http://fake.url").


