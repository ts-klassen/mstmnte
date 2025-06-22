mstmnte – Master & Maintenance helper library
============================================

Goal  
• Provide a *cowboy-agnostic* Erlang library to store and serve “master” reference
  data plus “maintenance” documents in CouchDB.  
• Ship ready-made API handlers and a tiny static Web UI, so an application that
  already runs Cowboy can expose

```
/mstmnte/master       – read-only reference data (IDs only)
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
    mstmnte.erl          (public helpers, DB bootstrap & type specs)
    mstmnte_handler.erl  (HTTP helpers – **no** Cowboy compile-time deps)
    mstmnte_route.erl    (generic Cowboy wrapper → {Mod,Fun}[,Extra])
    mstmnte_router.erl   (returns ready-made route lists)
    mstmnte_db.erl       (tiny façade hiding the klsn_db driver)
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
| GET    | /mstmnte/master              | List every master *ID*.                               |
| POST   | /mstmnte/master              | Body = list of IDs → return map *Id ➜ Doc*. If the    |
|        |                              | body is empty/missing, return *all* masters.          |
| GET    | /mstmnte/master/:id          | Single master; 404 if unknown.                        |

B. Maintenance endpoints:

| Method | Path                                | Semantics                                    |
|--------|-------------------------------------|----------------------------------------------|
| GET    | /mstmnte/maintenance                | List every maintenance *ID*.                 |
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

All DB I/O is funneled through this *very* small layer so the rest of the
code base can stay agnostic of the concrete CouchDB driver (`klsn_db`).  Its
surface is purposefully minimal:

```erlang
%% List every document ID stored in the database
-spec list(config()) -> {ok, [id()]} | {error, no_db}.

%% Fetch one document by ID
-spec get(id(), config()) -> {ok, doc()} | {error, no_db | not_found}.

%% Create or replace a document. Performs a basic optimistic-locking check
%% on the _rev field and returns `{error, conflict}` when the revision
%% mismatch cannot be resolved.
-spec upsert(doc(), config()) -> {ok, doc()} | {error, no_db | conflict}.

%% Ensure the configured database exists – idempotent.
-spec create_db(config()) -> ok.
```

`mstmnte_handler` converts those `{ok, …}` / `{error, …}` tuples into the
appropriate HTTP status codes and JSON responses.



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
