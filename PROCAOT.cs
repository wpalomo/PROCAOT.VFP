using System;
using Guineu;
using System.Windows.Forms;
 
namespace PROCAOT
{
class main
{
static void Main(string[] args)
{
Application.EnableVisualStyles();
GuineuInstance.InitInstance();
GuineuInstance.Do("st3menuv.fxp", args);
}
}
}	