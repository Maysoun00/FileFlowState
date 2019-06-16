using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Linq;
using TestHelper;
using DB;

namespace Test
{
    [TestClass]
    public class UnitTest : CodeFixVerifier
    {
        [TestMethod]
        public void TestDereferenceOnKnownNull()
        {
            var code = @"using System.Data.SqlClient;using System.IO;" +
@"
    public class Transaction
    {
        public void Commit(){
            return;
        }
        public void Rollback(){
                return;
        }
        public void Close(){
                return;
        }
    
    }
    public class DB
    {
        public void Open(){
            return;
        }
        public void Close(){
                return;
        }
        public Transaction BeginTransaction(string txt){
            Transaction tr = new Transaction();
            return tr;
        }
    
    }
    public class C
    {
        public DB db=new DB();

        public void M(){
            Transaction tr =  db.BeginTransaction(""SampleTransaction"");
            tr.Commit();
            tr.Rollback();
            db.Open();
            db.Close();

           
        }
    }
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(2, dx.Length);// one open transaction for func BeginTransaction, second for both rollback and commit.
            Assert.AreEqual(DBAnalyzer.PossibleEndOfScopeWithoutCloseId, dx[0].Id);
        }

        protected Diagnostic[] GetAnalyzerDiagnostics(string code)
        {
            var document = CreateDocument(code, LanguageNames.CSharp);
            var analyzer = new DBAnalyzer();
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
            return new DBAnalyzer();
        }
    }
}