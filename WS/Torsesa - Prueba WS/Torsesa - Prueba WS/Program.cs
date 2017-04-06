using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Torsesa___Prueba_WS.WSPrueba;
using System.Net;

namespace Torsesa___Prueba_WS
{
    class Program
    {
        static void Main(string[] args)
        {
            Program objMain = new Program();

            WSPrueba.PruebasSGA ws = new PruebasSGA();

            ws.Url = "http://62.82.59.121:7047/DynamicsNAV80/WS/CRONUS%20España%20S.A./Codeunit/PruebasSGA";            

            ws.UseDefaultCredentials = false;

            ws.Credentials = new NetworkCredential("Torsesa\\wsuser", "T0rs3s4");

            // Primero procesamos los Recadv que estén pendientes.
            System.Console.WriteLine("Lanzando el WS...");            

            // Primero procesamos los Recadv que estén pendientes.
            System.Console.WriteLine("La respuesta del WS es: " + ws.WSPrueba(155));

            System.Console.ReadLine();

        }
    }
}
