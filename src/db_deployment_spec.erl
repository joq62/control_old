-module(db_deployment_spec).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-record(deployment_spec,
	{
	  deployment_spec_id,
	  vsn,
	  restrictions,
	  services
	}).

-define(TABLE,deployment_spec).
-define(RECORD,deployment_spec).

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create(SpecId,Vsn,Restrictions,Services) ->
    Record=#deployment_spec{deployment_spec_id=SpecId,
			    vsn=Vsn,
			    restrictions=Restrictions,
			    services=Services
			   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{SpecId,Vsn,Restrictions,Services}||{?RECORD,SpecId,Vsn,Restrictions,Services}<-Z].



read(SpecId,Vsn) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.deployment_spec_id==SpecId,
		     X#?RECORD.vsn==Vsn])),
    [{XSpecId,XVsn,XRestrictions,XServices}||{?RECORD,XSpecId,XVsn,XRestrictions,XServices}<-Z].


delete(SpecId,Vsn) ->

    F = fun() -> 
		DeploymentSpec=[X||X<-mnesia:read({?TABLE,SpecId}),
			    X#?RECORD.deployment_spec_id==SpecId,X#?RECORD.vsn==Vsn],
		case DeploymentSpec of
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
