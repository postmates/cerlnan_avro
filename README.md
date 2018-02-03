# cerlnan_avro

[![Build Status](https://travis-ci.org/postmates/cerlnan_avro.svg?branch=master)](https://travis-ci.org/postmates/cerlnan_avro)

An Erlang client for Cernan's Avro source.

## Usage

The following examples assume you have an instance of Cernan's Avro source accepting connections on `localhost:2002`.

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

### Available Publication Options

As illustrated above, users can control various aspects of how their Avro payloads are published
by providing an optional `map()` to `publish`.

The following is a full listing of the options currently available by default:

| Option    | Type Spec             | Description                                                                       | Default               |
| :-------: | :-------------------: | :-------------------------------------------------------------------------------: | :-------------------: |
| sync      | boolean()             | Publish the given data synchronously?                                             | true                  |
| shard_by  | undefined or term()   | Value used to determine which shard / bucket the resulting Avro is published to.  | Random (undefined)    |

### Configuring Pools

`cerlnan_avro` publishes via `poolboy` backed connection pools.

#### Default Pool

By default, users are given a single pool which is locally addressable as `cerlnan_avro`.

Configuration of the default pool can be accomplished via either `application` or
OS environment.  The following table describes the options available and their default values:

| Application Variable  | Environment Variable          | Default       |
| :------------------:  | :---------------------------: | :-----------: |
| host                  | CERLNAN_AVRO_HOST             | localhost     |
| port                  | CERLNAN_AVRO_PORT             | 2002          |
| connect_timeout       | CERLNAN_AVRO_CONNECT_TIMEOUT  | 1000          |
| read_timeout          | CERLNAN_AVRO_READ_TIMEOUT     | 3000          |
| pool_size             | CERLNAN_AVRO_POOL_SIZE        | 10            |
| pool_overflow         | CERLNAN_AVRO_POOL_OVERFLOW    | 20            |

For example, the default pool could be explicitly configured by adding the following
entry to `sys.config`:

```erlang
{cerlnan_avro,
 [{pool_size, 10},
  {pool_overflow, 20},
  {host, "localhost"},
  {port, 2002},
  {connect_timeout, 1000},
  {read_timeout, 3000}]}
```

#### Custom Pools

Users can also create their own pools.

**Note** - When not using or configuring a pool named `cerlnan_avro` calls to `publish/3` and `publish/4` will
no longer work as they assume usage of the `cerlnan_avro` pool.  Variations of these calls which allow users to
select the pool to publish from are available.

The previous example is shorthand for:

```erlang
{cerlnan_avro,
   [{pools,
       [{cerlnan_avro,
         #{pool_size => 10,
           pool_overflow => 20,
           backend => cerlnan_avro_socket_v1,
           backend_args =>
               #{host => "localhost",
                 port => 2002,
                 connect_timeout => 1000,
                 read_timeout => 3000
                }}}
       ]}
   ]}
```

#### Pool Configs

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

## Developing

### Running Tests

```
make check
```

### Running Unit Tests

The following executes only eunit tests:

```
make unit
```

### Start an Interactive Shell

```
make shell
```
