# mstmnte – quick reference

This document summarises the current public interface and runtime
integration points for the **mstmnte** master-&-maintenance helper
library.

---

## 1. Options map (`opts()`)  
Defined and exported by `mstmnte`.

```erlang
-type opts() :: #{
        klsn_db := #{
            db_name := klsn_db:db(),   %% Default: <<"mstmnte">>
            db_info := klsn_db:info()  %% Default: #{} (COUCHDB_URL honoured)
        }
    }.
```

The *exact* same map is

* accepted by `mstmnte:init/1` (database bootstrap), and
* forwarded as the 3rd argument to every HTTP handler via the router.

---

## 2. Database bootstrap

Ensure CouchDB database exists **before** accepting requests.

```erlang
%% Using application env or defaults (see §4):
ok = mstmnte:init().

%% Explicit configuration
Cfg = #{klsn_db => #{db_name => <<"my_db">>,
                     db_info => #{url => <<"http://localhost:5984">>}}},
ok = mstmnte:init(Cfg).
```

The call delegates to `mstmnte_db:create_db/1`; if the DB already exists the
operation is a no-op.

---

## 3. Router helpers

```erlang
Routes = mstmnte_router:api() ++   % JSON API routes
         mstmnte_router:webui(),   % Static Web UI

Dispatch = cowboy_router:compile([{ '_', Routes }]).
```

`mstmnte_router` automatically merges defaults with any options found in the
application environment and embeds the resulting opts() map into every route
so that handlers receive consistent configuration.

---

## 4. Application environment

`sys.config` example:

```erlang
{mstmnte,
 [{klsn_db,
   #{db_name => <<"mstmnte_example">>,
     db_info => #{url => <<"http://localhost:5984">>}}}]}.
```

If omitted:

* `db_name`       → `<<"mstmnte">>`
* `db_info`       → `#{}` (and `klsn_db` will fall back to the `COUCHDB_URL`
  environment variable).

---

## 5. Public modules & responsibilities

| Module               | Responsibility |
|----------------------|----------------|
| `mstmnte`            | `init/0,1`, exported `opts/0` type |
| `mstmnte_db`         | CRUD helpers and `create_db/1` |
| `mstmnte_handler`    | HTTP action helpers (Cowboy-agnostic) |
| `mstmnte_route`      | Generic Cowboy handler wrapper |
| `mstmnte_router`     | Returns route lists (`api/0`, `webui/0`) |
| `priv/webui/*`       | Vanilla JS/HTML viewer |

---

## 6. Endpoints recap (all JSON unless noted)

| Method | Path                                   | Notes |
|--------|----------------------------------------|-------|
| GET    | `/mstmnte/master`                      | List master IDs |
| POST   | `/mstmnte/master`                      | Bulk fetch, body = list of IDs or `[]` for all |
| GET    | `/mstmnte/master/:id`                  | Single master |
| GET    | `/mstmnte/maintenance`                 | List maintenance IDs |
| GET    | `/mstmnte/maintenance/:id`             | Single maintenance document |
| PATCH  | `/mstmnte/maintenance`                 | Create/update document |
| GET    | `/mstmnte/webui/index.html` (static)   | Bundled GUI |

---

That’s all you need to integrate **mstmnte** into a Cowboy application and
have a working API + minimal web UI backed by CouchDB.
