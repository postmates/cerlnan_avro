# cerlnan_avro

An Erlang / Elixir client for Cernan's Avro source.

## Usage 

The following examples assume you have an instance of Cernan's Avro source accepting connections on `localhost:2002`.

### Erlang

```erlang
application:ensure_all_started(cerlnan_avro).

UserSchema =
    cerlnan_avro_schema:record(
        <<"User">>,
        [cerlnan_avro_schema:field(name, string),
         cerlnan_avro_schema:field(favorite_number, int),
         cerlnan_avro_schema:field(favorite_color, string)],
        [{namespace, 'example.avro'}]).
User1 = [{name, "Greg Wallice"}, {favorite_number, 10}, {favorite_color, "maroon"}].
User2 = [{name, "Alice Bob"}, {favorite_number, 32}, {favorite_color, "greenish-gold"}].
Users = [User1, User2].

%% Sync publish Users.
%% Blocks until Cernan acknowledges the publication.
ok = publish("example.avro.User", UserSchema, Users).

%% Async publish Users.
ok = publish("example.avro.User", UserSchema, Users, #{sync=>false}).
```

### Elixir

```elixir
schema = ExCernan.Avro.Schema.record(
  "User",
  [ExCernan.Avro.Schema.field("name", :string),
   ExCernan.Avro.Schema.field("favorite_number", :int),
   ExCernan.Avro.Schema.field("favorite_color", :string)],
 [{:namespace, "example.avro"}])

user1 = [{"name", "Foo Bar"}, {"favorite_number", 10}, {"favorite_color", "maroon"}]
user2 = [{"name", "Alice Bob"}, {"favorite_number", 32}, {"favorite_color", "greenish-gold"}]
users = [user1, user2]

# Sync publish users.
# Blocks until Cernan acknowledges the publication.
ExCernan.Avro.publish("example.avro.User", schema, users)

# Async publish users.
ExCernan.Avro.publish("example.avro.User", schema, users, %{:sync => false})
```

### Available Publication Options

As illustrated above, users can control various aspects of how their Avro payloads are published
by providing an optional `map()` to `publish`.

The following is a full listing of the options currently available by default:

| Option    | Type Spec             | Description                                                                       | Default               |
| :-------: | :-------------------: | :-------------------------------------------------------------------------------: | :-------------------: |
| sync      | boolean()             | Publish the given data synchronously?                                             | true                  |
| shard_by  | undefined or term()   | Value used to determine which shard / bucket the resulting Avro is published to.  | Random (undefined)    |

### Configuring Pools

`cerlnan_avro` and `ExCernan.Avro` both publish via `poolboy` backend connection pools.

By default, users are given a single pool which is locally addressable as `cerlnan_avro` initialized with
default arguments. However, users can make use of their `application` environment to configure their own pools.

**Note** - When not using or configuring a pool named `cerlnan_avro` calls to `publish/3` and `publish/4` will
no longer work as they assume usage of the `cerlnan_avro` pool.  Variations of these calls which allow users to
select the pool to publish from are available.

The following examples demonstrate reconfiguring the `cerlnan_avro` pool with 100 connections and 200 overflow workers:

#### Erlang

```erlang
[{cerlnan_avro,
    [{pools,
        [{cerlnan_avro,
          #{pool_size => 100,
            pool_overflow => 200
           }}]
     }]
  }]
```

#### Elixir

```elixir
    config :cerlnan_avro,
        pools: [
            {:cerlnan_avro,
             %{:pool_size => 100,
               :pool_overflow => 200
              }
            }]
```

#### Pool Config

Individual pool configs are expressed as `{name: atom(), options: map()}`.  The following options are available.

| Option        | Type Spec             | Description                                           | Default                   |
| :-----------: | :-------------------: | :---------------------------------------------------: | :-----------------------: |
| backend       | atom()                | Backend socket semantics.                             | cerlnan_avro_socket_v1    |
| backend_args  | map()                 | Initialization options for each worker in the pool.   | #{}                       |
| pool_size     | non_neg_integer()     | Number of pool workers to preallocate.                | 10                        |
| pool_overflow | non_neg_integer()     | Number of overflow workers to allow.                  | 20                        | 

##### Backend Options

By default, the following initialization options are available when specifying `backend_args`:

| Option            | Type Spec                                 | Description                               | Default               |
| :---------------: | :---------------------------------------: | :---------------------------------------: | :-------------------: |
| host              | inet:socket_address() or inet:hostname()  | Cernan host to connect to.                | Default = localhost   |
| port              | inet:port_number()                        | Port Cernan's Avro source is listening on.| Default = 2002        |
| connect_timeout   | timeout()                                 | Maximum time to wait connecting to Cernan.| Default = 1000        |
| read_timeout      | timeout()                                 | Maximum time to block on sync. publish.   | Default = 3000        |
