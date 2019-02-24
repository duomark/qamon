-module(qamon_dirtest).

-export([is_valid/0]).

%%% Validate the directory structure of the QA area.
-spec is_valid() -> boolean().
is_valid() ->
  lists:all(fun(Path) -> filelib:is_dir(Path) end, required_dirs()).

%%% All required directories must be present.
required_dirs() ->
  QA_Home = filename:join(["/Users", "qatest"]),
  [file:join(QA_Home, filename:join(Dir_Names))
   || Dir_Names <- qa_subdirs()].

qa_subdirs() ->
  [
   ["logs"],
   ["data"],
   ["data", "json"],
   ["data", "xml"]
  ].
