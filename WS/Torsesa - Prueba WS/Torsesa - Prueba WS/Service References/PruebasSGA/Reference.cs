﻿//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     Runtime Version:4.0.30319.42000
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

namespace Torsesa___Prueba_WS.PruebasSGA {
    
    
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ServiceModel.ServiceContractAttribute(Namespace="urn:microsoft-dynamics-schemas/codeunit/PruebasSGA", ConfigurationName="PruebasSGA.PruebasSGA_Port")]
    public interface PruebasSGA_Port {
        
        // CODEGEN: Generating message contract since the wrapper name (WSPrueba_Result) of message WSPrueba_Result does not match the default value (WSPrueba)
        [System.ServiceModel.OperationContractAttribute(Action="urn:microsoft-dynamics-schemas/codeunit/PruebasSGA:WSPrueba", ReplyAction="*")]
        Torsesa___Prueba_WS.PruebasSGA.WSPrueba_Result WSPrueba(Torsesa___Prueba_WS.PruebasSGA.WSPrueba request);
        
        [System.ServiceModel.OperationContractAttribute(Action="urn:microsoft-dynamics-schemas/codeunit/PruebasSGA:WSPrueba", ReplyAction="*")]
        System.Threading.Tasks.Task<Torsesa___Prueba_WS.PruebasSGA.WSPrueba_Result> WSPruebaAsync(Torsesa___Prueba_WS.PruebasSGA.WSPrueba request);
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.ServiceModel.MessageContractAttribute(WrapperName="WSPrueba", WrapperNamespace="urn:microsoft-dynamics-schemas/codeunit/PruebasSGA", IsWrapped=true)]
    public partial class WSPrueba {
        
        [System.ServiceModel.MessageBodyMemberAttribute(Namespace="urn:microsoft-dynamics-schemas/codeunit/PruebasSGA", Order=0)]
        public decimal importe;
        
        public WSPrueba() {
        }
        
        public WSPrueba(decimal importe) {
            this.importe = importe;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.ServiceModel.MessageContractAttribute(WrapperName="WSPrueba_Result", WrapperNamespace="urn:microsoft-dynamics-schemas/codeunit/PruebasSGA", IsWrapped=true)]
    public partial class WSPrueba_Result {
        
        [System.ServiceModel.MessageBodyMemberAttribute(Namespace="urn:microsoft-dynamics-schemas/codeunit/PruebasSGA", Order=0)]
        public string return_value;
        
        public WSPrueba_Result() {
        }
        
        public WSPrueba_Result(string return_value) {
            this.return_value = return_value;
        }
    }
    
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    public interface PruebasSGA_PortChannel : Torsesa___Prueba_WS.PruebasSGA.PruebasSGA_Port, System.ServiceModel.IClientChannel {
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    public partial class PruebasSGA_PortClient : System.ServiceModel.ClientBase<Torsesa___Prueba_WS.PruebasSGA.PruebasSGA_Port>, Torsesa___Prueba_WS.PruebasSGA.PruebasSGA_Port {
        
        public PruebasSGA_PortClient() {
        }
        
        public PruebasSGA_PortClient(string endpointConfigurationName) : 
                base(endpointConfigurationName) {
        }
        
        public PruebasSGA_PortClient(string endpointConfigurationName, string remoteAddress) : 
                base(endpointConfigurationName, remoteAddress) {
        }
        
        public PruebasSGA_PortClient(string endpointConfigurationName, System.ServiceModel.EndpointAddress remoteAddress) : 
                base(endpointConfigurationName, remoteAddress) {
        }
        
        public PruebasSGA_PortClient(System.ServiceModel.Channels.Binding binding, System.ServiceModel.EndpointAddress remoteAddress) : 
                base(binding, remoteAddress) {
        }
        
        [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
        Torsesa___Prueba_WS.PruebasSGA.WSPrueba_Result Torsesa___Prueba_WS.PruebasSGA.PruebasSGA_Port.WSPrueba(Torsesa___Prueba_WS.PruebasSGA.WSPrueba request) {
            return base.Channel.WSPrueba(request);
        }
        
        public string WSPrueba(decimal importe) {
            Torsesa___Prueba_WS.PruebasSGA.WSPrueba inValue = new Torsesa___Prueba_WS.PruebasSGA.WSPrueba();
            inValue.importe = importe;
            Torsesa___Prueba_WS.PruebasSGA.WSPrueba_Result retVal = ((Torsesa___Prueba_WS.PruebasSGA.PruebasSGA_Port)(this)).WSPrueba(inValue);
            return retVal.return_value;
        }
        
        [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
        System.Threading.Tasks.Task<Torsesa___Prueba_WS.PruebasSGA.WSPrueba_Result> Torsesa___Prueba_WS.PruebasSGA.PruebasSGA_Port.WSPruebaAsync(Torsesa___Prueba_WS.PruebasSGA.WSPrueba request) {
            return base.Channel.WSPruebaAsync(request);
        }
        
        public System.Threading.Tasks.Task<Torsesa___Prueba_WS.PruebasSGA.WSPrueba_Result> WSPruebaAsync(decimal importe) {
            Torsesa___Prueba_WS.PruebasSGA.WSPrueba inValue = new Torsesa___Prueba_WS.PruebasSGA.WSPrueba();
            inValue.importe = importe;
            return ((Torsesa___Prueba_WS.PruebasSGA.PruebasSGA_Port)(this)).WSPruebaAsync(inValue);
        }
    }
}
