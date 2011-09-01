-module(message).
-export([parse/1,convertToList/1,getRequestId/1]).

parse(Data) ->
  %io:format("data received:~p\n", [Data]),
  if 
     Data == [] ->
       more_read;
     Data /= [] ->  
       Index = string:str(Data, "\r\n\r\n"),
       if
          Index > 0 ->
            case  parseHP(Data, Index) of 
               {ok, RequestId, Payload, Remaining} ->
                 {ok, RequestId, convertToList(Payload), Remaining};
               more_read ->
                 more_read;
               error ->
                 error
            end     
       end
  end.     


parseHP(Data, Index) ->
  H = string:substr(Data, 1, Index -1),
  %io:format("header is:~p\n", [H]),
  CL = getContentLength(H),
  ReqId = getRequestId(H),
  io:format("ReqId=~p\n", [ReqId]),
  Total = string:len(Data),
  %io:format("total=~p, Index=~p contentLength=~p\n", [Total,Index, CL]),
  if 
     CL > 0 ->
        if
          Total >= (Index - 1 + 4 + CL) ->
            {ok, ReqId, string:substr(Data, Index+4, CL), string:substr(Data, Index+4+CL)};
          Total < (Index -1 + 4 + CL) -> % need more reading
            more_read
        end;
     CL == 0 ->
       error
  end.

getContentLength(Hs) ->
  Index = string:str(Hs, "\r\n"),
  if 
     Index > 0 ->
       Header = string:substr(Hs, 1, Index-1),
       case getLengthFromHeader(Header) of 
          nextHeader ->
             getLengthFromHeader(string:sub_string(Hs, Index+2));
          Other ->
             Other
        end;     
     Index == 0 ->
       Header = Hs,
       case getLengthFromHeader(Header) of
          nextHeader ->
             0;
          Other ->
             Other
       end      
  end.     

getLengthFromHeader(H) ->
   Index = string:str(H, "Content-Length"),
   if 
     Index > 0 ->  % find header with requestId 
        Pos = string:str(H, ":"),
        if
          Pos > 0 ->
             list_to_integer(string:sub_string(H, Pos+1));
          Pos == 0 ->
             0
        end;
     Index == 0 ->
        nextHeader
   end.

getRequestId(Hs) ->
  Index = string:str(Hs, "\r\n"),
  if 
     Index > 0 ->
       Header = string:substr(Hs, 1, Index-1),
       case getReqFromHeader(Header) of 
          nextHeader ->
             getRequestId(string:sub_string(Hs, Index+2));
          Other ->
             Other
        end;     
     Index == 0 ->
       Header = Hs,
       case getReqFromHeader(Header) of
          nextHeader ->
             "";
          Other ->
             Other
       end      
  end.     

getReqFromHeader(H) ->
   Index = string:str(H, "Request-Id"),
   if 
     Index > 0 ->  % find header with requestId 
        Pos = string:str(H, ":"),
        if
          Pos > 0 ->
             string:sub_string(H, Pos+1);
          Pos == 0 ->
             ""
        end;
     Index == 0 ->
        nextHeader
   end.

convertToList(Jdata) ->
  Term = json_eep:json_to_term(Jdata),
  {Items} = Term,
  Values = getValueList(Items),
  getRealValue(Values, []).

getRealValue([], Sum) ->
  Sum;
getRealValue([H|T], Sum) ->
  {Item} = H,
  V = getValueFromItem(Item),
  if
     V == [] ->
       getRealValue(T, Sum);
     V /= [] ->
       if
          Sum == [] ->
            getRealValue(T, [V]);
          Sum /= [] ->
            getRealValue(T, [V|Sum])
       end     
  end.

getValueFromItem([]) ->
  [];
getValueFromItem([H|T]) ->
  {Key, Value} = H,
  case Key of
     <<"value">> ->
       Value;
     _ -> 
       getValueFromItem(T)
  end.   

getValueList([]) ->
  [];
getValueList([H|T]) ->
  {Key, Value} = H,
  case Key of
     <<"data">> ->
       Value;
     _ -> 
       getValueList(T)
   end.  
