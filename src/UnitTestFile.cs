using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Linq;
using TestHelper;
using File;

namespace Test
{
    [TestClass]
    public class UnitTest : CodeFixVerifier
    {
        [TestMethod]
        public void TestDereferenceOnKnownNull()
        {
            var code = @"using System.IO;" +
           @"
public class C
{
    public FileStream fs = null;
    public FileStream ds ;
  
  public void M()
  {
        fs = File.Create(""C:\\Users\\SALMAAB\\Desktop\\ReadMe.txt"");
        fs.Close();
        fs.Read(null,0,0);
        ds = File.Create(""C:\\Users\\SALMAAB\\Desktop\\ReadMe.txt"");
        ds.Write(null,0,0);
        ds.Close();
        ds.Write(null,0,0);
  }  
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(4, dx.Length);
            //1. file fs is not closed in M because read change the state to opened . 2. file ds not close in M same because the write.
            //3.read after close in line 29. 4. write after close in line 32.
            Assert.AreEqual(FileAnalyzer.PossibleUnClosedFileId, dx[0].Id);
            Assert.AreEqual(FileAnalyzer.PossibleUnClosedFileId, dx[1].Id);
            Assert.AreEqual(FileAnalyzer.PossibleReadWithoutOpenId, dx[2].Id);
            Assert.AreEqual(FileAnalyzer.PossibleWriteWithoutOpenId, dx[3].Id);
        }

        protected Diagnostic[] GetAnalyzerDiagnostics(string code)
        {
            var document = CreateDocument(code, LanguageNames.CSharp);
            var analyzer = new FileAnalyzer();
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
            return new FileAnalyzer();
        }
    }
}