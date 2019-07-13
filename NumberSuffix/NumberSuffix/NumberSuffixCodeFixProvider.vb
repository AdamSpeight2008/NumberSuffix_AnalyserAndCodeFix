Option Strict On
Imports System
Imports System.Collections.Generic
Imports System.Collections.Immutable
Imports System.Composition
Imports System.Linq
Imports System.Threading
Imports System.Threading.Tasks
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CodeFixes
Imports Microsoft.CodeAnalysis.CodeActions
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports Microsoft.CodeAnalysis.Rename
Imports Microsoft.CodeAnalysis.Text


<ExportCodeFixProvider(LanguageNames.VisualBasic, Name:=NameOf(NumberSuffixCodeFixProvider)), [Shared]>
Public Class NumberSuffixCodeFixProvider
  Inherits CodeFixProvider

  Private Const title As String = "Add Number Suffix"

  Public NotOverridable Overrides ReadOnly Property FixableDiagnosticIds As ImmutableArray(Of String)
    Get
      Return ImmutableArray.Create(NumberSuffixAnalyzer.DiagnosticId)
    End Get
  End Property

  Public NotOverridable Overrides Function GetFixAllProvider() As FixAllProvider
    Return WellKnownFixAllProviders.BatchFixer
  End Function

  Public NotOverridable Overrides Async Function RegisterCodeFixesAsync(context As CodeFixContext) As Task
    Dim root = Await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(False)
    Dim diagnostic = context.Diagnostics.First()
    Dim diagnosticSpan = diagnostic.Location.SourceSpan
    ' Find the type statement identified by the diagnostic.
    Dim declaration = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType(Of LiteralExpressionSyntax)().First()
    ' Register a code action that will invoke the fix.
    context.RegisterCodeFix(
        CodeAction.Create(
            title:=title,
            createChangedDocument:=Function(c) ApplyFix(context.Document, declaration, c),
            equivalenceKey:=title),
        diagnostic)
  End Function

  Private Async Function ApplyFix(document As Document, literal As LiteralExpressionSyntax, cancellationToken As CancellationToken) As Task(Of Document)
    If literal Is Nothing Then Return document
    Dim parentOfLiteral = literal.Parent
    If parentOfLiteral Is Nothing Then Return document
    dim parentBinaryExpression = TryCast(parentOfLiteral, BinaryExpressionSyntax)
    If parentBinaryExpression Is Nothing Then Return document
    Dim semanticModel =await document.GetSemanticModelAsync(cancellationToken) 
    If semanticModel is Nothing Then Return document
    Dim targetTypeInfo as TypeInfo
    If parentBinaryExpression.Left.Equals(literal) Then
       targetTypeInfo = semanticModel.GetTypeInfo(parentBinaryExpression.Right, cancellationToken)
    Elseif parentBinaryExpression.Right.Equals(literal) Then
       targetTypeInfo = semanticModel.GetTypeInfo(parentBinaryExpression.Left, cancellationToken)
    Else
      Return document
    End If
    Dim typeSuffix = NumberSuffixAnalyzer.GetTypeSuffix(targetTypeInfo.Type)
    If typeSuffix Is Nothing Then Return document
    Dim oldTree = Await document.GetSyntaxTreeAsync
    If oldTree Is Nothing Then Return document
    Dim oldRoot = Await oldTree.GetRootAsync(cancellationToken)
    If oldRoot Is Nothing Then Return document
    Dim newLiteral = SyntaxFactory.ParseExpression(literal.Token.ValueText & typeSuffix).
                       WithLeadingTrivia(literal.GetLeadingTrivia).
                       WithTrailingTrivia(literal.GetTrailingTrivia)
    Dim newRoot = oldRoot.ReplaceNode(literal,newLiteral)
    Return document.WithSyntaxRoot(newRoot)
  End Function
End Class
