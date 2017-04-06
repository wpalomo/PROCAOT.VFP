
*>-------------------------------------------------------------------
*> Test de Web Service del WS al SGA - Torsesa.
*>-------------------------------------------------------------------

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

*>>>>>>>> Uso Test.

Set Step On

Try
	*> oWS = NewObject("WSImport.WSIClass", "http://192.168.1.18:88/WSFoxPro/WS.asmx", "Torsesa\wsuser", "T0rs3s4")

	oWS = NewObject("WSImport.WSIClass")
	? oWS.SetCredenciales("http://192.168.1.18:88/WSFoxPro/WS.asmx", "Torsesa/wsuser", "T0rs3s4")
Set Step On

	? oWS.ReceiveTest()
Set Step On

	? oWS.ReceiveArticulo("A2", ;
	                      "Descri A222", ;
	                      "84A1", ;
	                      "F001", ;
	                      "U001", ;
	                      "S", ;
	                      "30", ;
	                      "1", ;
	                      "20", ;
	                      "15", ;
	                      "0", ;
	                      "0", ;
	                      "GENE", ;
	                      "S", "3","3","3","3","GENE", "S","S000")

	? oWS.ReceiveCodBar("A2", "84A2")
set step on

	? oWS.ReceiveProveedor("TEST", ;
	                      "PROVEEDOR TEST", ;
	                      "N43212", ;
	                      "POL. IND. LA PERA", ;
	                      "LEPE", ;
	                      "0008", ;
	                      "99001", ;
	                      "954555666", ;
	                      "954999888")
set step on

	? oWS.ReceivePedidoCompra("0000000001", ;
	                      "N", ;
	                      "1000", ;
	                      "201510011", ;
	                      "TEST", ;
	                      "0010", ;
	                      "A2", ;
	                      "100", ;
	                      "30102015", ;
	                      "0000", ;
	                      "XXXX", "XXXXXXXXXX", "0000", "0")
set step on

	? oWS.ReceiveDevolucionCliente("0000000001", ;
	                      "N", ;
	                      "1500", ;
	                      "201510011", ;
	                      "0010", ;
	                      "A2", ;
	                      "100", ;
	                      "30102015", ;
	                      "000000", ;
	                      "2345679", ;
	                      "PRODUCTO EN MAL ESTADO", ;
	                      "2000", ;
	                      "23041590")
set step on

	? oWS.RequestStock("0000000001", ;
	                   "0001", ;
	                   "EA549969", ;
	                   "L1", ;
	                   "100", ;
	                   Dtoc(Date()), ;
	                   Ttoc(Datetime()), ;
	                   "X")
set step on

	? oWS.ReceivePedidoVentaCab("0000000002", ;
								"N", ;
								"2000", ;
								"20151001", ;
								"21092015", ;
								"30102015", ;
								"000001010", ;
								"P400", ;
								"000000", ;
								"0", ;
								"10", ;
								"P", ;
								"0000")
	set step on

	? oWS.ReceivePedidoVentaObs("0000000002", ;
								"2000", ;
								"20151001", ;
								"0000", ;
								"OBSERVACIONES GENERALES")
			  
set step on

	? oWS.ReceivePedidoVentaCli("0000000002", ;
								"2000", ;
								"20151001", ;
								"0003219", ;
								"PERICO DE LOS PALOTES", ;
								"PRIMERA DIRECCIÓN", ;
								"SEGUNDA DIRECCIÓN", ;
								"ÚLTIMA DIRECCIÓN", ;
								"333222222-X", ;
								"FUENLABRADA (MADRID)", ;
								"28940", ;
								"0008")

set step on

	? oWS.ReceivePedidoVentaDet("0000000002", ;
								"2000", ;
								"20151001", ;
								"0010", ;
								"A1", ;
								" ", ;
								"100", ;
								"0000", ;
								" ", ;
								"N")

set step on

	? oWS.ReceivePedidoVentaDet("0000000002", ;
								"2000", ;
								"20151001", ;
								"0020", ;
								"A2", ;
								" ", ;
								"100", ;
								"0000", ;
								" ", ;
								"S")

set step on

	? oWS.ReceiveBloqueo("1000", "13097314     ")
	? oWS.ReceiveBloqueo("2000", "20151001     ")

set step on

	cMessage = "Mje No. " + Alltrim(Str(oWS.StatusCode)) + Chr(13) + Chr(10)
	cMessage = cMessage + "Mje: " + Mline(oWS.StatusText, 1)
	cTitle = oWS.Result

Catch To oErr
	cMessage = "Error No. " + Alltrim(Str(oErr.ErrorNo)) + Chr(13) + Chr(10)
	cMessage = cMessage + "Error: " + oErr.Message
	cTitle = "ERROR"

Finally
	=MessageBox(cMessage, 48, cTitle)

EndTry

Release oWS
