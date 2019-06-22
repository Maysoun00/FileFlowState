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
using System.Reflection;
using System.IO;
using System.Data;
namespace DB
{
    internal class DBFlowState : FlowState
    {
        private readonly SemanticModel model;
        private readonly ImmutableDictionary<object, DBState> variableStates;

        public DBFlowState(SemanticModel model)
            : this(model, ImmutableDictionary.Create<object, DBState>(new VariableComparer(model)))
        {
        }

        private DBFlowState(SemanticModel model, ImmutableDictionary<object, DBState> variableStates)
        {
            this.model = model;
            this.variableStates = variableStates;
        }

        public ImmutableDictionary<object, DBState> VariableStates
        {
            get
            {
                return variableStates;
            }
        }

        private DBFlowState With(ImmutableDictionary<object, DBState> newVariableStates)
        {
            if (this.variableStates != newVariableStates)
            {
                return new DBFlowState(this.model, newVariableStates);
            }
            else
            {
                return this;
            }
        }

        public override bool Equals(FlowState state)
        {
            var nfs = state as DBFlowState;
            return nfs != null && nfs.variableStates == this.variableStates;
        }

        public override FlowState Join(FlowState state)
        {
            var nfs = (DBFlowState)state;
            var joinedVariableStates = this.variableStates;

            Join(this.variableStates, nfs.variableStates, ref joinedVariableStates);
            Join(nfs.variableStates, this.variableStates, ref joinedVariableStates);

            return this.With(joinedVariableStates);
        }

        private void Join(
            ImmutableDictionary<object, DBState> branchA,
            ImmutableDictionary<object, DBState> branchB,
            ref ImmutableDictionary<object, DBState> joined)
        {
            // for all items in a
            foreach (var kvp in branchA)
            {
                DBState bs;
                if (!branchB.TryGetValue(kvp.Key, out bs))
                {
                    bs = GetDeclaredState(kvp.Key);
                }

                var w = Join(kvp.Value, bs);

                joined = joined.SetItem(kvp.Key, w);
            }
        }

        private DBState Join(DBState a, DBState b)
        {    

            return DBState.Unknown;
        }

        public override FlowState After(SyntaxNode node)
        {
            // variables can change state after assignment
            var assign = node as AssignmentExpressionSyntax;
            if (assign != null)
            {
                return this.WithReferenceState(assign.Left, this.GetReferenceState(assign.Right));
            }

            // variables acquire initial state from initializer
            var declarator = node as VariableDeclaratorSyntax;
            if (declarator != null)
            {
                if (declarator.Initializer != null)
                {
                    var symbol = this.model.GetDeclaredSymbol(node);
                    if (symbol != null)
                    {
                        return this.WithReferenceState(symbol, GetReferenceState(declarator.Initializer.Value));
                    }
                }
            }

            var invocation = node as InvocationExpressionSyntax;

            if (invocation != null && this.model.GetSymbolInfo(((MemberAccessExpressionSyntax)invocation.Expression).Expression).Symbol.Kind != SymbolKind.NamedType)
            {
                return this.WithReferenceState(((MemberAccessExpressionSyntax)invocation.Expression).Expression, this.GetReferenceState(invocation));
            }

            if (this.IsConditional(node))
            {
                FlowState trueState;
                FlowState falseState;
                this.AfterConditional(node, out trueState, out falseState);
                return trueState.Join(falseState);
            }

            return this;
        }

        public override bool IsConditional(SyntaxNode node)
        {
            switch (node.Kind())
            {
                case SyntaxKind.EqualsExpression:
                case SyntaxKind.NotEqualsExpression:
                    return true;
                default:
                    return false;
            }
        }

        public override void AfterConditional(SyntaxNode node, out FlowState trueState, out FlowState falseState)
        {
            trueState = this;
            falseState = this;

            var kind = node.Kind();
            if (kind == SyntaxKind.EqualsExpression || kind == SyntaxKind.NotEqualsExpression)
            {
                var binop = (BinaryExpressionSyntax)node;

                ExpressionSyntax influencedExpr = null;
                if (binop.Right.IsKind(SyntaxKind.NullLiteralExpression))
                {
                    influencedExpr = this.GetVariableExpression(binop.Left);
                }
                else if (binop.Left.IsKind(SyntaxKind.NullLiteralExpression))
                {
                    influencedExpr = this.GetVariableExpression(binop.Right);
                }

            }
        }

        public DBFlowState WithReferenceState(ISymbol symbol, DBState state)
        {
            switch (symbol.Kind)
            {
                case SymbolKind.Local:
                case SymbolKind.Parameter:
                case SymbolKind.RangeVariable:
                    return this.With(this.variableStates.SetItem(symbol.OriginalDefinition, state));
                default:
                    return this;
            }
        }

        public DBFlowState WithReferenceState(ExpressionSyntax expr, DBState state)
        {
            var variable = GetVariableExpression(expr);
            if (variable != null)
            {
                var expSymbol = this.model.GetSymbolInfo(variable).Symbol;
                if (expSymbol != null)
                {
                    return this.With(this.variableStates.SetItem(expSymbol.OriginalDefinition, state));
                }
            }

            return this;
        }

        /// <summary>
        /// Returns the portion of the expression that represents the variable
        /// that can be tracked, or null if the expression is not trackable.
        /// </summary>
        private ExpressionSyntax GetVariableExpression(ExpressionSyntax expr)
        {
            expr = WithoutParens(expr);

            switch (expr.Kind())
            {
                // assignment expressions yield their LHS variable for tracking
                // this comes into play during null checks: (x = y) != null
                // in this case x can be assigned tested-not-null state.. (what about y?)
                case SyntaxKind.SimpleAssignmentExpression:
                    return GetVariableExpression(((BinaryExpressionSyntax)expr).Left);

                // all dotted names are trackable.
                case SyntaxKind.SimpleMemberAccessExpression:
                case SyntaxKind.PointerMemberAccessExpression:
                case SyntaxKind.QualifiedName:
                case SyntaxKind.IdentifierName:
                case SyntaxKind.AliasQualifiedName:
                    return expr;

                default:
                    return null;
            }
        }

        private ExpressionSyntax WithoutParens(ExpressionSyntax expr)
        {
            while (expr.IsKind(SyntaxKind.ParenthesizedExpression))
            {
                expr = ((ParenthesizedExpressionSyntax)expr).Expression;
            }

            return expr;
        }

        public DBState GetAssignmentState(ExpressionSyntax variable, bool isInvocationParameter = false)
        {
            var symbol = this.model.GetSymbolInfo(variable).Symbol;
            if (symbol != null)
            {
                return GetAssignmentState(symbol, isInvocationParameter);
            }
            else
            {
                return DBState.Unknown;
            }
        }

        public DBState GetAssignmentState(ISymbol symbol, bool isInvocationParameter = false)
        {
            switch (symbol.Kind)
            {
                case SymbolKind.Local:
                    return DBState.Unknown;
                case SymbolKind.Parameter:
                    if (!isInvocationParameter)
                    {
                        // method body parameters get their state assigned just like locals
                        return DBState.Unknown;
                    }
                    else
                    {
                        goto default;
                    }
                default:
                    return GetDeclaredState(symbol);
            }
        }

        public DBState GetReferenceState(ExpressionSyntax expression)
        {
            if (expression != null)
            {
                expression = WithoutParens(expression);

                var expSymbol = this.model.GetSymbolInfo(expression).Symbol;
                DBState state;
                if (expSymbol != null && this.variableStates.TryGetValue(expSymbol.OriginalDefinition, out state))
                {
                    return state;
                }

                switch (expression.Kind())
                {

                    case SyntaxKind.NullLiteralExpression:
                        return DBState.Unknown;

                    case SyntaxKind.InvocationExpression:
                        var ourMethodName = ((MemberAccessExpressionSyntax)((InvocationExpressionSyntax)expression).Expression).Name;
                        var invocation = expression as InvocationExpressionSyntax;
                        var methodSymbol = (IMethodSymbol)(model.GetSymbolInfo(invocation).Symbol ?? model.GetDeclaredSymbol(invocation));
                        var declaringTypeName = string.Format(
                             "{0}.{1}",
                            methodSymbol.ContainingNamespace.ToString(),
                            methodSymbol.ContainingType.Name
                        );
                        var methodName = methodSymbol.Name;
                     
                       if (declaringTypeName == "<global namespace>.DB" && ourMethodName.Identifier.ValueText.StartsWith("Open"))
                       {
                             return DBState.Connected;
                       }
                       if (declaringTypeName == "<global namespace>.DB" && ourMethodName.Identifier.ValueText.StartsWith("Close"))
                       {
                             return DBState.Closed;
                       }
                       if (declaringTypeName == "<global namespace>.DB" && ourMethodName.Identifier.ValueText.StartsWith("Close"))
                       {
                             return DBState.Closed;
                       }
                       if (declaringTypeName == "global namespace>.DB" && ourMethodName.Identifier.ValueText.StartsWith("Open"))
                       {
                             return DBState.Connected;
                       }

                        return GetReferenceState(((MemberAccessExpressionSyntax)((InvocationExpressionSyntax)expression).Expression).Expression);

                    
                    case SyntaxKind.SimpleMemberAccessExpression:
                        return GetReferenceState(((MemberAccessExpressionSyntax)expression).Expression);
                }

                var symbol = this.model.GetSymbolInfo(expression).Symbol;
                if (symbol != null)
                {
                    return GetReferenceState(symbol);
                }
            }

            return DBState.Unknown;
        }
        public DBState GetReferenceState(ISymbol symbol)
        {
            DBState state;
            if (this.variableStates.TryGetValue(symbol.OriginalDefinition, out state))
            {
                return state;
            }

            return GetDeclaredState(symbol);
        }

        public DBState GetDeclaredState(object symbolOrSyntax)
        {
            var syntax = symbolOrSyntax as ExpressionSyntax;
            if (syntax != null)
            {
                return GetDeclaredState(syntax);
            }

            var symbol = symbolOrSyntax as ISymbol;
            if (symbol != null)
            {
                return GetDeclaredState(symbol);
            }

            return DBState.Unknown;
        }

        public DBState GetDeclaredState(ExpressionSyntax syntax)
        {
            var symbol = this.model.GetSymbolInfo(syntax).Symbol;
            if (symbol != null)
            {
                return GetDeclaredState(symbol);
            }
            else
            {
                return DBState.Unknown;
            }
        }

        public static DBState GetDeclaredState(ISymbol symbol)
        {
            switch (symbol.Kind)
            {
                case SymbolKind.Local:
                    return DBState.Unknown;

                default:
                    return GetSymbolInfo(symbol).DBState;
            }
        }


        private class SymbolInfo
        {
            public readonly DBState DBState;

            public SymbolInfo(DBState defaultState)
            {
                this.DBState = defaultState;
            }
        }

        private static ConditionalWeakTable<ISymbol, SymbolInfo> symbolInfos
            = new ConditionalWeakTable<ISymbol, SymbolInfo>();

        private static SymbolInfo GetSymbolInfo(ISymbol symbol)
        {
            SymbolInfo info;
            if (!symbolInfos.TryGetValue(symbol, out info))
            {
                info = CreateSymbolInfo(symbol);
                info = symbolInfos.GetValue(symbol, _ => info);
            }

            return info;
        }

        private static SymbolInfo CreateSymbolInfo(ISymbol symbol)
        {
            
            return new SymbolInfo(DBState.Unknown);
        }

        private static ITypeSymbol GetVariableType(ISymbol symbol)
        {
            switch (symbol.Kind)
            {
                case SymbolKind.Parameter:
                    return ((IParameterSymbol)symbol).Type;

                case SymbolKind.Local:
                    return ((ILocalSymbol)symbol).Type;

                case SymbolKind.Field:
                    return ((IFieldSymbol)symbol).Type;

                case SymbolKind.Property:
                    return ((IPropertySymbol)symbol).Type;

                case SymbolKind.Method:
                    return ((IMethodSymbol)symbol).ReturnType;

                default:
                    return null;
            }
        }

        internal class VariableComparer : IEqualityComparer<object>
        {
            private readonly SemanticModel model;

            public VariableComparer(SemanticModel model)
            {
                this.model = model;
            }

            public new bool Equals(object x, object y)
            {
                if (x == y)
                {
                    return true;
                }

                if (x == null || y == null)
                {
                    return false;
                }

                var xs = x as ISymbol;
                var ys = y as ISymbol;

                var xn = x as ExpressionSyntax;
                var yn = y as ExpressionSyntax;

                if (xs == null && xn != null)
                {
                    xs = this.model.GetSymbolInfo(xn).Symbol;
                }

                if (ys == null && yn != null)
                {
                    ys = this.model.GetSymbolInfo(yn).Symbol;
                }

                if (xs.Equals(ys))
                {
                    // don't need to compare syntax to match these (or static symbols)
                    if (xs.Kind == SymbolKind.Local || xs.Kind == SymbolKind.Parameter || xs.Kind == SymbolKind.RangeVariable || xs.IsStatic)
                    {
                        return true;
                    }

                    // syntax must be similar to be confident this reaches the same instance
                    return xn != null && yn != null && SyntaxFactory.AreEquivalent(xn, yn, topLevel: false);
                }

                return false;
            }

            public int GetHashCode(object obj)
            {
                // hash code is based on symbol's hash code
                var sym = obj as ISymbol;
                var exp = obj as ExpressionSyntax;

                if (sym == null && exp != null)
                {
                    sym = this.model.GetSymbolInfo(exp).Symbol;
                }

                if (sym != null)
                {
                    return sym.GetHashCode();
                }

                return obj.GetHashCode();
            }
        }
    }
}