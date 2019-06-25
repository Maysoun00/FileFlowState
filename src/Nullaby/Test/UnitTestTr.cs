using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Linq;
using TestHelper;
using Transaction;

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
        public void Connect(){
                return;
        }
    
    }
   
    public class C
    {
        public Transaction tr = null;
        public Transaction tr2;
        public void M(){
            tr.Connect();
            tr2.Connect();
            tr.Commit();
            tr.Rollback();
            
           
        }
    }
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(2, dx.Length);// one open Transaction and not doing commit/rollback, second for both rollback and commit.
            Assert.AreEqual(TransactionAnalyzer.PossibleNotDoingRollbackOrCommitId, dx[0].Id);
            Assert.AreEqual(TransactionAnalyzer.PossibleTwiceRollbackOrCommitId, dx[1].Id);
        }

        protected Diagnostic[] GetAnalyzerDiagnostics(string code)
        {
            var document = CreateDocument(code, LanguageNames.CSharp);
            var analyzer = new TransactionAnalyzer();
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
            return new TransactionAnalyzer();
        }
    }
}