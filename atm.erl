-module(atm).
-export([widthdraw/2]).

widthdraw(A, []) -> throw("No money");
widthdraw(A, B) -> 
	SortedBanknotes = lists:sort(fun(X,Y) -> X > Y end, B),
	Vars = getMoney(A, SortedBanknotes),
	Len = length(Vars),
	if
		Len == 0 -> 
			{'request_another_amount', [], SortedBanknotes};
		Len > 0 ->
			F = fun(X,Y) -> length(X) < length(Y) end,
			[BestVariant|_] = lists:sort(F, Vars),
			SortedBestVariant = lists:sort(fun(X,Y) -> X > Y end, BestVariant),
			RestBanknotes = getRestBanknotes(BestVariant, SortedBanknotes),
			{'ok', SortedBestVariant, RestBanknotes}
	end.

	
		
% Функция получения вариантов размена денег
% A - Amount
% R - restAmount
% B - banknotes

getMoney(0,_) -> [[]];
getMoney(_,[]) -> [];
getMoney(R,_) when R < 0 -> [];
getMoney(R,[FirstB|LastB]) when R > 0 ->	
	Variants1 = [[FirstB] ++ X || X <- getMoney(R - FirstB, LastB)], 
	Variants2 = getMoney(R, LastB),
	Variants1 ++ Variants2.

getRestBanknotes([],R) -> R;
getRestBanknotes([FirstU|LastU], [FirstA|LastA]) ->
	if
		FirstU == FirstA -> getRestBanknotes(LastU, LastA);
		FirstU /= FirstA -> [FirstA] ++ getRestBanknotes([FirstU|LastU], LastA)
	end.
