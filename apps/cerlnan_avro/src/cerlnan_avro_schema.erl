-module(cerlnan_avro_schema).

% This module serves to prevent erlavro from leaking
% into downstream publishers.

-include_lib("erlavro/include/erlavro.hrl").

-export([record/2, record/3,
         enum/2, enum/3,
         field/2, field/3,
         array/1, array/2,
         map/1, map/2,
         union/1]).

%%====================================================================
%% Types
%%====================================================================

-type name() :: avro:name().
-type name_raw() :: atom() | string() | binary().
-type type() :: avro:avro_type().
-type type_or_name() :: type() | name_raw().
-opaque record() :: avro:record_type().
-type record_opt_name() :: doc | namespace | aliases.
-type record_opt() :: {record_opt_name(), term()}.
-opaque field() :: avro:record_field().
-type field_opt_name() :: doc | order | default | aliases.
-type field_opt() :: {field_opt_name(), term()}.
-opaque avro_map() :: #avro_map_type{}.
-type custom_prop() :: {binary(), number() | binary() | [binary()]}.
-opaque array() :: avro:array_type().
-opaque union() :: avro:union_type().
-opaque enum() :: avro:enum_type().
-type symbol() :: atom() | binary().
-type type_prop() :: record_opt_name() | binary().

-export_type(
   [name/0,
    name_raw/0,
    type/0,
    type_or_name/0,
    record/0,
    field/0,
    avro_map/0,
    array/0,
    union/0,
    enum/0
   ]).

%%====================================================================
%% API
%%====================================================================

-spec record(name(), [field()]) -> record().
record(Name, Fields) ->
    record(Name, Fields, []).

-spec record(name_raw(), [field()], [record_opt()]) -> record().
record(Name, Fields, Options) ->
    avro_record:type(Name, Fields, Options).

-spec field(name_raw(), type() | name_raw()) -> field().
field(Name, TypeOrName) ->
    field(Name, TypeOrName, []).

-spec field(name_raw(), type() | name_raw(), [field_opt()]) -> field().
field(Name, TypeOrName, Options) ->
    avro_record:define_field(Name, TypeOrName, Options).

-spec array(type()) -> array().
array(Type) ->
    array(Type, []).

-spec array(type(), [custom_prop()]) -> array().
array(Type, Props) ->
    avro_array:type(Type, Props).

-spec map(type()) -> avro_map().
map(Type) ->
    map(Type, []).

-spec map(type(), [custom_prop()]) -> avro_map().
map(Type, Props) ->
    avro_map:type(Type, Props).

-spec union([type() | name()]) -> union().
union(TypesOrNames) ->
    avro_union:type(TypesOrNames).

-spec enum(name(), [symbol()]) -> enum().
enum(Name, Symbols) ->
    enum(Name, Symbols, []).

-spec enum(name(), [symbol()], [type_prop()]) -> enum().
enum(Name, Symbols, Opts) ->
    avro_enum:type(Name, Symbols, Opts).
