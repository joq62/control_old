-module(db_service_discovery).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-record(service_discovery,
	{
	  id,
	  vsn,
	  vm
	}).

-define(TABLE,service_discovery).
-define(RECORD,service_discovery).

start() ->
  %  mnesia:create_schema([node()]), %Should be started by db_mnesia
  %  mnesia:start(),
%    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
 %   mnesia:wait_for_tables(?TABLE, 20000).   %Should be started by db_mnesia
    ok.
create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag}]),
    mnesia:wait_for_tables(?TABLE, 20000).

create(Record) ->
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
  do(qlc:q([X || X <- mnesia:table(?TABLE)])).



read(ServiceId) ->
    X=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.id==ServiceId])),
    [Vm||{?RECORD,_ServiceId,_Vsn,Vm}<-X].

read(ServiceId,Vsn) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.id==ServiceId,
		     X#?RECORD.vsn==Vsn])),
    [Vm||{?RECORD,_ServiceId,_Vsn,Vm}<-Z].

delete(Id,Vsn,Vm) ->
    F = fun() -> 
		ServiceDiscovery=[X||X<-mnesia:read({?TABLE,Id}),
				     X#?RECORD.id==Id,
				     X#?RECORD.vsn==Vsn,
				     X#?RECORD.vm==Vm],
		case ServiceDiscovery of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1) 
		end
	end,
    mnesia:transaction(F).


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
