
Define Class ShowCustomers AS Session OLEPUBLIC

   PROCEDURE CustomersInGermany AS String
      LOCAL loXMLAdapter AS XMLAdapter
      LOCAL lcXMLCustomers AS String

      loXMLAdapter = CREATEOBJECT("XMLAdapter")
      
      Open Database "C:\Program Files (x86)\Microsoft Visual FoxPro 9\Samples\Northwind\northwind.dbc"

      USE customers
      SELECT * ;
         FROM customers ;
         WHERE country LIKE "Germany%" ;
         INTO CURSOR curCustomers

      loXMLAdapter.AddTableSchema("curCustomers")
      loXMLAdapter.UTF8Encoded = .T.
      loXMLAdapter.ToXML("lcXMLCustomers")

      CLOSE DATABASES ALL

      RETURN lcXMLCustomers
   ENDPROC

EndDefine
