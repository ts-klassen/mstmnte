mstmnte – Master & Maintenance helper library
============================================

Goal  
• Provide a *cowboy-agnostic* Erlang library to store and serve “master” reference
  data plus “maintenance” documents in CouchDB.  
• Ship ready-made API handlers and a tiny static Web UI, so an application that
  already runs Cowboy can expose

```
/mstmnte/master       – read-only reference data
/mstmnte/maintenance  – CRUD documents that include metadata/payload
/mstmnte/…(static)    – vanilla HTML/JS viewer (optional)
```

without having to write any boiler-plate.



1.  OTP application
-------------------

• Application name: **mstmnte**  
• Code layout

```
mstmnte/
  src/
    mstmnte_app.erl          (application callback)
    mstmnte_sup.erl          (top-level supervisor)
    mstmnte_handler.erl      (HTTP entry points – no cowboy deps)
    mstmnte_router.erl       (router helpers – returns cowboy routes)
    mstmnte_db.erl           (↔ klsn_db façade, *returns todo* for now)
  priv/
    webui/                   (index.html + *.css/*.js)
```

Only two external deps:

```
{jsone, ">= 1.7.0"},
{klsn,  ">= 0.1.0"}   %% transitively gives us klsn_db
```

Cowboy is **not** a compile-time dependency.



2.  Data model
--------------

CouchDB database: one fixed DB per deployment.  
Name is supplied once via the `Opts` map (see §4).

Maintenance document shape (example):

```erlang
#{
  <<"_id">>     := <<"maintenance_42">>,
  <<"_rev">>    := <<"1-abc">>,      %% when returned by the API
  metadata      := #{...},           %% user-defined, any map()
  payload       := #{...}            %% ditto
}
```

The library does **no validation**.  
Conflicts (`409`) are surfaced as-is; the caller decides how to resolve them.



3.  HTTP contract
-----------------

Base prefix is always `/mstmnte`.

A. Master endpoints (read-only):

| Method | Path                         | Semantics                                             |
|--------|------------------------------|-------------------------------------------------------|
| GET    | /mstmnte/master              | List every master document (full objects).            |
| POST   | /mstmnte/master              | Body = list of IDs → return map *Id ➜ Doc*. If the    |
|        |                              | body is empty/missing, return *all* masters.          |
| GET    | /mstmnte/master/:id          | Single master; 404 if unknown.                        |

B. Maintenance endpoints:

| Method | Path                                | Semantics                                    |
|--------|-------------------------------------|----------------------------------------------|
| GET    | /mstmnte/maintenance                | List every maintenance document (full obj).  |
| GET    | /mstmnte/maintenance/:id            | Fetch one document; 404 if unknown.          |
| PATCH  | /mstmnte/maintenance                | Create or update. Body must include `_id`.   |
|        |                                     | Any of `_rev`, `metadata`, `payload` may be  |
|        |                                     | omitted → existing values are kept.          |

All payloads are `application/json`.  
Encoding/decoding via `jsone` with default options.



4.  Library call surface (Erlang)
---------------------------------

### 4.1 Handler helpers

These are what a Cowboy handler’s `init/2` typically delegates to.

```
-spec mstmnte_handler:list(Req, State)    -> {Response, Req, State}.
-spec mstmnte_handler:get(Req, State)     -> ...
-spec mstmnte_handler:bulk_get(Req, State)-> ...
-spec mstmnte_handler:maint_list(Req,State)-> ...
-spec mstmnte_handler:maint_get(Req,State) -> ...
-spec mstmnte_handler:maint_patch(Req,State)-> ...

%% All take an optional Opts map as *third* argument if you prefer:
mstmnte_handler:list(Req, State, Opts).    %% Opts may contain {klsn_db, DbOpts}
```

`Opts`  
• `klsn_db` – map passed verbatim to `klsn_db`.  
  Default: `#{}` (klsn_db will fall back to its own env config).

### 4.2 Router helpers

```
mstmnte_router:api()  -> Routes       %% API endpoints only
mstmnte_router:webui()-> Routes       %% Static WebUI
```

Both return lists ready to be concatenated into your Cowboy router
(expected format for Cowboy 2.11: `{Verb, Path, Handler, []}` tuples or
`{"/mstmnte/[...]", cowboy_static, ...}` for assets).

Example integration:

```erlang
Dispatch = cowboy_router:compile(
             mstmnte_router:api() ++
             mstmnte_router:webui() ++
             MyOtherRoutes),
{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{env => #{dispatch => Dispatch}}).
```



5.  Static Web UI
-----------------

Located under `priv/webui/`.

• `index.html` lists masters & maintenance docs via `fetch()` calls to the
  corresponding API endpoints.  
• No external CDNs; ships minified CSS/JS in the same directory.  
• Pure vanilla JS + simple Flexbox layout – easy to replace.



6.  `mstmnte_db` façade
-----------------------

All DB I/O is funneled through this module so the library remains ignorant of
the underlying driver.  **Current stub behaviour** (to be implemented later):

```
list_master(DbOpts)               -> todo.
bulk_get_master(Ids, DbOpts)      -> todo.
get_master(Id, DbOpts)            -> todo.

list_maintenance(DbOpts)          -> todo.
get_maintenance(Id, DbOpts)       -> todo.
patch_maintenance(Doc, DbOpts)    -> todo.
```

The handlers only pattern-match on `{ok, Data}` / `{error, Reason}` tuples and
convert them into the proper HTTP replies.



7.  Usage cookbook
------------------

1. Add to your rebar or mix config:

```erlang
{deps, [mstmnte]}.
```

2. Wire the routes (see §4.2).  
3. In your Cowboy handler, delegate:

```erlang
init(Req, State) ->
    mstmnte_handler:list(Req, State).      %% or whichever handler you need
```

4. Optionally pass DB options:

```erlang
init(Req, State) ->
    Opts = #{klsn_db => #{dbname => <<"mydb">>, auth => {user, pass}}},
    mstmnte_handler:maint_patch(Req, State, Opts).
```



8.  Future extension points
---------------------------

• Pagination / filtering for large datasets.  
• Authentication plug-in.  
• `-spec` types once the public surface stabilises.  
• EUnit / Common Test suite with dockerised CouchDB fixture.



That should give you a clear picture of the planned skeleton and its behaviour.
Let us know if you’d like any adjustments before we start implementing!
