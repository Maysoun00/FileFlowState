using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Linq;
using TestHelper;
using Null;

namespace Test
{
    [TestClass]
    public class UnitTest : CodeFixVerifier
    {
        [TestMethod]
        public void TestDereferenceOnKnownNull()
        {
            var code = @"using System.IO;" +
           @"public class Nullaby
    {
        public void Connect(){
            return;
        }
        public void Open(){
                return;
        }
        public void Close(){
                return;
        }
    
    }
public class C
{
    public Nullaby n;
    public Nullaby n2;
  public void M()
  {
        n2.Connect();
        n2.Close();
        n2.Connect();

        n.Open();
        n.Close();
        n.Close();
  }  
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(2, dx.Length);// one doing twice close so the second one is null.close, the second one is open on unknown
            Assert.AreEqual(NullAnalyzer.PossibleSendingNullToFuncId, dx[0].Id);
        }

        protected Diagnostic[] GetAnalyzerDiagnostics(string code)
        {
            var document = CreateDocument(code, LanguageNames.CSharp);
            var analyzer = new NullAnalyzer();
            var sys = document.Project.AddMetadataReference(MetadataReference.CreateFromFile(@"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\System.dll"));
            var compilerDiagnostics = sys.AddMetadataReference(MetadataReference.CreateFromFile(@"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\System.Data.dll")).GetCompilationAsync().Result.GetDiagnostics();
            //var compilerDiagnostics = document.Project.GetCompilationAsync().Result.GetDiagnostics();
            bool hasCompileErrors = compilerDiagnostics.Any(d => d.Severity == DiagnosticSeverity.Error);
            Assert.AreEqual(hasCompileErrors, false, "Compilation error(s)");

            return GetSortedDiagnosticsFromDocuments(analyzer, new[] { document });
        }

        protected override CodeFixProvider GetCSharpCodeFixProvider()
        {
            throw new NotImplementedException();
        }

        protected override DiagnosticAnalyzer GetCSharpDiagnosticAnalyzer()
        {
            return new NullAnalyzer();
        }
    }
}