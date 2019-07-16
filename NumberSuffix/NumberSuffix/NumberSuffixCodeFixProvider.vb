Option Strict On
Imports System.Collections.Immutable
Imports System.Composition
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CodeFixes
Imports Microsoft.CodeAnalysis.CodeActions
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

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
             equivalenceKey:=title), diagnostic)
  End Function

  Private Async Function ApplyFix(document As Document, literal As LiteralExpressionSyntax, cancellationToken As CancellationToken) As Task(Of Document)
    If literal Is Nothing OrElse literal.Parent Is Nothing Then Return document

    Dim semanticModel = Await document.GetSemanticModelAsync(cancellationToken)
    If semanticModel Is Nothing Then Return document

    Dim targetTypeInfo As TypeInfo
    If NumberSuffixAnalyzer.TryGetTypeFromParentBinaryExpression(literal.Parent, literal, semanticModel, cancellationToken, targetTypeInfo).NOR(
       NumberSuffixAnalyzer.TryGetTypeInfoFromParentAssignment(literal.Parent, semanticModel, cancellationToken, targetTypeInfo)) Then Return document

    Dim typeSuffix As String = Nothing
    If Not NumberSuffixAnalyzer.TryGetTypeSuffix(targetTypeInfo.Type, typeSuffix) Then Return document
    Dim oldTree = Await document.GetSyntaxTreeAsync
    If oldTree Is Nothing Then Return document
    Dim oldRoot = Await oldTree.GetRootAsync(cancellationToken)
    If oldRoot Is Nothing Then Return document
    Dim newLiteral = SyntaxFactory.ParseExpression(literal.Token.ValueText & typeSuffix).WithTriviaFrom(literal)
    Dim newRoot = oldRoot.ReplaceNode(literal, newLiteral)
    Return document.WithSyntaxRoot(newRoot)
  End Function

End Class
