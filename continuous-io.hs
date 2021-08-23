--This module is written as an exercise in Haskell IO.
--Basic IO functions are written here.
--These functions can be used in competetive programming environments,
--	although the code has been tested exclusively with Hackerrank only.

--This module exports several functions capable of taking input from several lines as and when necessary,
--	hence the name 'continuous IO'.

--All these functions have a similar signature:
--	(<Some other params here>)-> initial_string::String-> IO (<Whatever you want>,residual_string::String)

module ContinuousIO
(
	getNextWord,
	getNStringsAsList,
	getNStringsAsMappedList,
	getNIntsAsList,
	get2IntsAsTuple,
	get3IntsAsTuple,
	get4IntsAsTuple,
)
where

import Data.Char

--Given a string, get the next word(continuous sequence of characters) out of it.
--The next word is returned, alongwith the remainder of the string.
--If the string doesn't have any more words, perform an IO operation (getLine) and read in the next line.
--This function will block until one word is read.
getNextWord::String->IO (String,String)
--If the string is empty, perform an IO operation and process it. This part is blocking.
getNextWord ""	=
	getLine >>= (\line->getNextWord line)
--If we have atleast 1 character, do
getNextWord str	=
	--If there is a leading space, ignore it and repeat the operation on the rest of the string.
	--If there is no leading space, get the first word.
	if isSpace (head str) then getNextWord (tail str) else return (_getNextWord str)
	where
	--Given a string, get the next word(continuous sequence of characters) out of it.
	--The next word is returned, alongwith the remainder of the string.
	--If the string doesn't have any more words, return nothing.
	--This function is non-blocking.
	_getNextWord::String->(String,String)
	--If the string is empty, return nothing.
	_getNextWord ""=("","")
	--The string has atleast 1 character.
	--If the first character is a space, then everything after that is the rest of the string.
	--If it isn't a space, then append that character to whatever sequence we got from the rest of the string.
	_getNextWord (ch:sx)=
		if isSpace ch then ("",(ch:sx)) else ((ch:seq),rest)
		where
		(seq,rest)=_getNextWord sx;

--Read the next N Strings and make a list of Strings out of them.
--The residual string is also returned.
getNStringsAsList::Int->String->IO ([String],String)
getNStringsAsList 0 str	=return ([],str)
getNStringsAsList n str	=
	getNextWord str >>= (\(w,rest)->
		getNStringsAsList (n-1) rest >>= (\(list,rest)->
			return ((w:list),rest)
		)
	)

--Read the next N Strings and make a list of Strings out of them.
--Apply the mapping function to that list as well.
--The residual string is also returned.
getNStringsAsMappedList::Int->(String->x)->String->IO ([x],String)
getNStringsAsMappedList n fn str=
	getNStringsAsList n str >>= (\(list,rest)->
		return (map fn list,rest)
	)

--Read the next N Strings and make a list of Ints out of them.
--The residual string is also returned.
getNIntsAsList::Int->String->IO ([Int],String)
getNIntsAsList n str=
	getNStringsAsMappedList n (\str->(read str::Int)) str

--Read 2 Ints and return a tuple out of them.
get2IntsAsTuple::String->IO ((Int,Int),String)
get2IntsAsTuple str=
	getNIntsAsList 2 str >>= (\((a:b:_),rest)->
		return ((a,b),rest)
	)

--Read 3 Ints and return a tuple out of them.
get3IntsAsTuple::String->IO ((Int,Int,Int),String)
get3IntsAsTuple str=
	getNIntsAsList 3 str >>= (\((a:b:c:_),rest)->
		return ((a,b,c),rest)
	)

--Read 4 Ints and return a tuple out of them.
get4IntsAsTuple::String->IO ((Int,Int,Int,Int),String)
get4IntsAsTuple str=
	getNIntsAsList 4 str >>= (\((a:b:c:d:_),rest)->
		return ((a,b,c,d),rest)
	)

