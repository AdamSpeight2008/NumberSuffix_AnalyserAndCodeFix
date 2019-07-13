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
    ' See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/FixAllProvider.md for more information on Fix All Providers
    Return WellKnownFixAllProviders.BatchFixer
  End Function

  Public NotOverridable Overrides Async Function RegisterCodeFixesAsync(context As CodeFixContext) As Task
    Dim root = Await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(False)

    ' TODO: Replace the following code with your own analysis, generating a CodeAction for each fix to suggest

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
    Dim p = literal.Parent
    If p Is Nothing THen return document
    dim pe = TryCast(p, BinaryExpressionSyntax)
    If pe is Nothing THen REturn document
    Dim sm =await document.GetSemanticModelAsync(cancellationToken) 
    If sm is Nothing then return document
    Dim ct as TypeInfo
    If pe.Left.Equals(literal) Then
       ct = sm.GetTypeInfo(pe.Right, cancellationToken)
    Elseif pe.Right.Equals(literal) Then
       ct = sm.GetTypeInfo(pe.Left, cancellationToken)
    Else
      Return document
    End If
    Dim ts = NumberSuffixAnalyzer.GetTypeSuffix(ct.Type)
    If ts Is Nothing Then REturn document
    dim oldTree = Await document.GetSyntaxTreeAsync
    If oldTree Is Nothing Then Return document
    dim oldRoot = Await oldTree.GetRootAsync(cancellationToken)
    If oldRoot Is Nothing Then Return document
   Dim newLiteral = SyntaxFactory.ParseExpression(literal.Token.ValueText & ts).WithLeadingTrivia(literal.GetLeadingTrivia).WithTrailingTrivia(literal.GetTrailingTrivia())
    dim newRoot = oldRoot.ReplaceNode(literal,newLiteral)
    Return document.WithSyntaxRoot(newRoot)
  End Function
End Class
