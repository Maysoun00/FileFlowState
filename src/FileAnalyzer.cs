using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using System.IO;
namespace File
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class FileAnalyzer : DiagnosticAnalyzer
    {

      public const string PossibleReadWithoutOpenId = "NN0001";
      public const string PossibleUnClosedFileId = "NN0002";
      public const string PossibleWriteWithoutOpenId = "NN0003";

      internal static DiagnosticDescriptor PossibleReadWithoutOpen =
          new DiagnosticDescriptor(
              id: PossibleReadWithoutOpenId,
              title: "Possible read of scope without open it",
              messageFormat: "possible trying to read IO Stream file without opened it.",
              category: "File",
              defaultSeverity: DiagnosticSeverity.Warning,
              isEnabledByDefault: true);
      internal static DiagnosticDescriptor PossibleUnClosedFile =
          new DiagnosticDescriptor(
              id: PossibleUnClosedFileId,
              title: "Possible end of block without closing a file",
              messageFormat: "possible end of block with file unclosed.",
              category: "File",
              defaultSeverity: DiagnosticSeverity.Warning,
              isEnabledByDefault: true);
      internal static DiagnosticDescriptor PossibleWriteWithoutOpen =
          new DiagnosticDescriptor(
              id: PossibleWriteWithoutOpenId,
              title: "Possible write of scope without open it",
              messageFormat: "possible trying writing to IO Stream file without opened it.",
              category: "File",
              defaultSeverity: DiagnosticSeverity.Warning,
              isEnabledByDefault: true);

        private static readonly ImmutableArray<DiagnosticDescriptor> s_supported =
            ImmutableArray.Create(PossibleReadWithoutOpen, PossibleUnClosedFile, PossibleWriteWithoutOpen);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
        {
            get { return s_supported; }
        }

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterCodeBlockAction(AnalyzeCodeBlock);
        }

        private static void AnalyzeCodeBlock(CodeBlockAnalysisContext context)
        {
            new CodeBlockAnalyzer(context).Analyze(context.CodeBlock);
        }

        private class CodeBlockAnalyzer : CSharpSyntaxWalker
        {
            private readonly CodeBlockAnalysisContext context;
            private FlowAnalysis<FileFlowState> flowAnalysis;

            public CodeBlockAnalyzer(CodeBlockAnalysisContext context)
            {
                this.context = context;
            }

            public void Analyze(SyntaxNode node)
            {
                // do null flow analysis
                var flowAnalzyer = new FlowAnalyzer<FileFlowState>(this.context.SemanticModel, new FileFlowState(this.context.SemanticModel));
                this.flowAnalysis = flowAnalzyer.Analyze(node);

                // check assignments and dereferences and report diagnostics
                this.Visit(node);

                var state = this.flowAnalysis.GetFlowState(node);
                foreach(var variableState in state.VariableStates)
                {
                     if(variableState.Value == FileState.Unknown || variableState.Value == FileState.Opened)
                     {
                          context.ReportDiagnostic(Diagnostic.Create(PossibleUnClosedFile, node.GetLocation()));
                     }
                }

            }

            private FileState GetReferenceState(ExpressionSyntax expression)
            {
                var state = this.flowAnalysis.GetFlowState(expression);
                return state.GetReferenceState(expression);
            }

            public override void VisitVariableDeclarator(VariableDeclaratorSyntax node)
            {
                base.VisitVariableDeclarator(node);

                if (node.Initializer != null)
                {
                    var symbol = context.SemanticModel.GetDeclaredSymbol(node);
                    if (symbol != null)
                    {
                        CheckAssignment(symbol, node.Initializer.Value);
                    }
                }
            }

            public override void VisitEqualsValueClause(EqualsValueClauseSyntax node)
            {
                base.VisitEqualsValueClause(node);

                if (node.Parent != null && node.Parent.IsKind(SyntaxKind.PropertyDeclaration))
                {
                    var symbol = context.SemanticModel.GetDeclaredSymbol((PropertyDeclarationSyntax)node.Parent);
                    if (symbol != null)
                    {
                        CheckAssignment(symbol, node.Value);
                    }
                }
            }

            public override void VisitArrowExpressionClause(ArrowExpressionClauseSyntax node)
            {
                base.VisitArrowExpressionClause(node);

                // check for mismatch in property value expressions not matching property declaration
                if (node.Parent != null && node.Parent.IsKind(SyntaxKind.PropertyDeclaration))
                {
                    switch (node.Parent.Kind())
                    {
                        case SyntaxKind.PropertyDeclaration:
                        case SyntaxKind.IndexerDeclaration:
                            var symbol = context.SemanticModel.GetDeclaredSymbol(node.Parent);
                            if (symbol != null)
                            {
                                CheckAssignment(symbol, node.Expression);
                            }
                            break;
                    }
                }
            }

            public override void VisitParameter(ParameterSyntax node)
            {
                base.VisitParameter(node);

                // check for parameter defaults not matching parameter declaration
                if (node.Default != null)
                {
                    var symbol = context.SemanticModel.GetDeclaredSymbol(node);
                    if (symbol != null)
                    {
                        CheckAssignment(symbol, node.Default.Value, isInvocationParameter: true);
                    }
                }
            }

            public override void VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
            {
                base.VisitMemberAccessExpression(node);

            }

            public override void VisitAssignmentExpression(AssignmentExpressionSyntax node)
            {
                base.VisitAssignmentExpression(node);
                CheckAssignment(node.Left, node.Right);
            }

            private void CheckAssignment(ExpressionSyntax variable, ExpressionSyntax expression)
            {
                var state = this.flowAnalysis.GetFlowState(expression);
                var exprState = state.GetReferenceState(expression);
                CheckAssignment(state.GetAssignmentState(variable), exprState, expression);
            }

            private void CheckAssignment(ISymbol symbol, ExpressionSyntax expression, bool isInvocationParameter = false)
            {
                var state = this.flowAnalysis.GetFlowState(expression);
                var exprState = state.GetReferenceState(expression);
                CheckAssignment(state.GetAssignmentState(symbol, isInvocationParameter), exprState, expression);
            }

            private void CheckAssignment(FileState variableState, FileState expressionState, ExpressionSyntax expression)
            {
                
            }

            public override void VisitReturnStatement(ReturnStatementSyntax node)
            {
                if (node.Expression != null)
                {
                    var symbol = context.SemanticModel.GetEnclosingSymbol(node.SpanStart);
                    if (symbol != null)
                    {
                        CheckAssignment(symbol, node.Expression);
                    }
                }

                base.VisitReturnStatement(node);
            }

            public override void VisitInvocationExpression(InvocationExpressionSyntax node)
            {
                base.VisitInvocationExpression(node);
   
                // check for possible dereference of null on member access (dot)
                var state = this.flowAnalysis.GetFlowState(node.Expression);
                var method = context.SemanticModel.GetSymbolInfo(node).Symbol as IMethodSymbol;
                switch (state.GetReferenceState(((MemberAccessExpressionSyntax)node.Expression).Expression))
                {

                      case FileState.Unknown:
                           if (method.Name.StartsWith("Read"))
                            {
                             context.ReportDiagnostic(Diagnostic.Create(PossibleReadWithoutOpen, node.GetLocation()));
                            }
                           if (method.Name.StartsWith("Write"))
                            {
                             context.ReportDiagnostic(Diagnostic.Create(PossibleWriteWithoutOpen, node.GetLocation()));
                            }
                            break;
                      case FileState.Opened:
                            break;
                      case FileState.Closed:
                           if (method.Name.StartsWith("Read"))
                            {
                             context.ReportDiagnostic(Diagnostic.Create(PossibleReadWithoutOpen, node.GetLocation()));
                            }
                           if (method.Name.StartsWith("Write"))
                            {
                             context.ReportDiagnostic(Diagnostic.Create(PossibleWriteWithoutOpen, node.GetLocation()));
                            }
                            break;
                }

                
            }

            public override void VisitObjectCreationExpression(ObjectCreationExpressionSyntax node)
            {
                base.VisitObjectCreationExpression(node);

                var method = context.SemanticModel.GetSymbolInfo(node).Symbol as IMethodSymbol;
                if (method != null)
                {
                    CheckArguments(node.ArgumentList.Arguments, method.Parameters);
                }
            }

            public override void VisitConstructorInitializer(ConstructorInitializerSyntax node)
            {
                base.VisitConstructorInitializer(node);

                var method = context.SemanticModel.GetSymbolInfo(node).Symbol as IMethodSymbol;
                if (method != null)
                {
                    CheckArguments(node.ArgumentList.Arguments, method.Parameters);
                }
            }

            private void CheckArguments(SeparatedSyntaxList<ArgumentSyntax> arguments, ImmutableArray<IParameterSymbol> parameters)
            {
                // check parameter assignments from arguments
                if (arguments.Count <= parameters.Length)
                {
                    for (int i = 0; i < arguments.Count; i++)
                    {
                        CheckAssignment(parameters[i], arguments[i].Expression, isInvocationParameter: true);
                    }
                }
            }
        }
    }
}
