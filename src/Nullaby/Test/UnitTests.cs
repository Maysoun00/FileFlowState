using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Linq;
using TestHelper;
using Nullaby;

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
  
  public void M()
  {
    fs = File.Create(""C:\\Users\\SALMAAB\\Desktop\\ReadMe.txt"");
   
    int i = 0;

    if ( i == 0 && true ) {
    fs.Close();
    //fs.ReadByte();
    //fs.WriteByte(0);	
    //fs.Read(null,0,0);
    fs.WriteAsync(null,0,0);


  }


    }
   
}
";
            var dx = GetAnalyzerDiagnostics(code);
            Assert.AreEqual(1, dx.Length);
            Assert.AreEqual(FileAnalyzer.PossibleReadWithoutOpentId, dx[0].Id);
        }

        protected Diagnostic[] GetAnalyzerDiagnostics(string code)
        {
            code = code + @"";
            var document = CreateDocument(code, LanguageNames.CSharp);
            var analyzer = new FileAnalyzer();

            var compilerDiagnostics = document.Project.GetCompilationAsync().Result.GetDiagnostics();
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