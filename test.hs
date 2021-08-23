import ContinuousIO

test1::IO ()
test1=
	get4IntsAsTuple "" >>= (\(tuple,rest)->
		print tuple >>= (\_->print rest)
	);

test2::IO ()
test2=do

	(w1,rest)	<-	getNextWord ""
	(w2,rest)	<-	getNextWord rest
	(w3,rest)	<-	getNextWord rest
	print (w1,w2,w3,rest)

test3::IO ()
test3=
	getNIntsAsList 3 "" >>= (\(list,rest)->
		print list >>= (\_->print rest)
	);

main::IO()
main=
	test2

--		123  45   6			21	0  

--		ABC  DE   F			GH	I  
