%%% -------------------------------------------------------------------
%%% @author : joqerlang
%%% @doc : ets dbase for master service to manage app info , catalog  
%%%
%%% -------------------------------------------------------------------
-module(deployment).
 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%-compile(export_all).
-export([create_spec/4,
	 read_spec/2,
	 delete_spec/2]).

%% ====================================================================
%% External functions
%% ====================================================================


%% --------------------------------------------------------------------
%% Function:create(ServiceId,Vsn,HostId,VmId)
%% Description: Starts vm and deploys services 
%% Returns: ok |{error,Err}
%
%% --------------------------------------------------------------------
create_spec(AppId,AppVsn,Restriction,ServiceList)->
    Reply=case if_db:deployment_spec_read(AppId,AppVsn) of
	      []->
		  if_db:deployment_spec_create(AppId,AppVsn,Restriction,ServiceList),
		  ok;
	      Err->
		  {error,[already_defined,Err,AppId,AppVsn]}
	  end,
    Reply.

read_spec(AppId,AppVsn)->
    if_db:deployment_spec_read(AppId,AppVsn).

delete_spec(AppId,AppVsn)->
    if_db:deployment_spec_delete(AppId,AppVsn).



%% --------------------------------------------------------------------
%% 
%%
%% --------------------------------------------------------------------
