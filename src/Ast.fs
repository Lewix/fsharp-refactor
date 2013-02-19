namespace FSharpRefactor.Engine.Ast

open System
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

module Ast =
    type AstNode =
        | Expression of SynExpr
        | Pattern of SynPat
        | SimplePatterns of SynSimplePats
        | SimplePattern of SynSimplePat
        | ModuleOrNamespace of SynModuleOrNamespace
        | ModuleDeclaration of SynModuleDecl
        | Binding of SynBinding
        | File of ParsedImplFileInput
        | Ident of Ident
        | MatchClause of SynMatchClause
        | TypeDefinition of SynTypeDefn
        | TypeDefinitionRepresentation of SynTypeDefnRepr
        | MemberDefinition of SynMemberDefn

    let MakeAstNode (tree : ParsedInput option) =
        match tree with
            | Some(ParsedInput.ImplFile(f)) -> Some(AstNode.File(f))
            | _ -> raise (new NotImplementedException("Use an impl file instead of a sig file"))

    let getParseTree source = 
        let checker = InteractiveChecker.Create(NotifyFileTypeCheckStateIsDirty(fun _ -> ()))
        let filename = "test.fs"
        let options source = checker.GetCheckOptionsFromScriptRoot(filename, source, DateTime.Now, [| |])
        checker.UntypedParse(filename, source, options source).ParseTree
        
    let Parse source = MakeAstNode (getParseTree source)

    // Active patterns to make dealing with the syntax tree more convenient
    let (|ModuleOrNamespaceChildren|_|) (expression : SynModuleOrNamespace) =
        match expression with
            | SynModuleOrNamespace (_,_,e1,_,_,_,_) -> Some(e1)

        
    let (|SynExprChildren|_|) (expression : SynExpr) =
        match expression with
            | SynExpr.Paren(e1,_,_,_) -> Some([e1])
            | SynExpr.Quote(e1,_,e2,_,_) -> Some([e1;e2])
            | _ -> raise (new NotImplementedException("Add a new entry to the active pattern for SynExpr children:" + (string expression)))

    let (|Children|_|) (node : AstNode) =
        match node with
            | File(ParsedImplFileInput(_,_,_,_,_,ns,_)) -> Some(List.map AstNode.ModuleOrNamespace ns)
            | Pattern(p) ->
                match p with
                    | SynPat.Or(p1,p2,_) -> Some [Pattern p1; Pattern p2]
                    | SynPat.Named(p2,_,_,_,_) -> Some [Pattern(p2)]
                    | SynPat.Wild(_) -> None
                    | SynPat.LongIdent(LongIdentWithDots(is,_),_,_,ps,_,_) -> Some(List.append (List.map AstNode.Ident is) (List.map AstNode.Pattern ps))
                    | SynPat.Paren(p,_) -> Some([AstNode.Pattern p])
                    | SynPat.Tuple(ps,_) -> Some(List.map AstNode.Pattern ps)
                    | SynPat.Const(_,_) -> None
                    | SynPat.Typed(p,_,_) -> Some([AstNode.Pattern p])
                    | SynPat.ArrayOrList(_,ps,_) -> Some(List.map AstNode.Pattern ps)
                    | _ -> raise (new NotImplementedException("Add a new entry to pattern for Pattern: " + (string p)))
            | SimplePattern(p) ->
                match p with
                    | SynSimplePat.Id(i,_,_,_,_,_) -> Some([AstNode.Ident i])
                    | SynSimplePat.Typed(p,_,_) -> Some([AstNode.SimplePattern p])
                    | SynSimplePat.Attrib(p,_,_) -> Some([AstNode.SimplePattern p])
            | SimplePatterns(p) ->
                match p with
                    | SynSimplePats.SimplePats(ps,_) -> Some(List.map AstNode.SimplePattern ps)
                    | SynSimplePats.Typed(p,_,_) -> Some([AstNode.SimplePatterns p])
            | ModuleOrNamespace(ns) ->
                match ns with
                    | ModuleOrNamespaceChildren(modules) -> Some(List.map AstNode.ModuleDeclaration modules)
                    | _ -> None
            | ModuleDeclaration(m) ->
                match m with
                    | SynModuleDecl.Let(_,bs,_) -> Some(List.map AstNode.Binding bs)
                    | SynModuleDecl.DoExpr(_,e,_) -> Some([AstNode.Expression e])
                    | SynModuleDecl.Open(_,_) -> None
                    | SynModuleDecl.NestedModule(_,ds,_,_) -> Some(List.map AstNode.ModuleDeclaration ds)
                    | SynModuleDecl.Types(ts,_) -> Some(List.map AstNode.TypeDefinition ts)
                    | _ -> raise (new NotImplementedException("Add a new entry to pattern for ModuleDeclaration: " + (string m)))
            | Binding(b) ->
                match b with
                    | SynBinding.Binding(_,_,_,_,_,_,_,p,_,e,_,_) -> Some([AstNode.Pattern p; AstNode.Expression e])

            | Expression(e) ->
                match e with
                    | SynExpr.LongIdentSet(_,e,_)
                    | SynExpr.Typed(e,_,_)
                    | SynExpr.TypeApp(e,_,_,_,_,_,_)
                    | SynExpr.Downcast(e,_,_)
                    | SynExpr.YieldOrReturn(_,e,_)
                    | SynExpr.Paren(e,_,_,_)
                    | SynExpr.ArrayOrListOfSeqExpr(_,e,_)
                    | SynExpr.New(_,_,e,_)
                    | SynExpr.DotGet(e,_,_,_)
                    | SynExpr.CompExpr(_,_,e,_) -> Some([AstNode.Expression e])
                    | SynExpr.ArrayOrList(_,es,_)
                    | SynExpr.Tuple(es,_,_) -> Some(List.map AstNode.Expression es)
                    | SynExpr.Record(_,copyExpr,fieldValues,_) ->
                        let expressions =
                            (Option.map fst copyExpr)::(List.map (fun (_,e,_) -> e) fieldValues)
                            |> List.map Option.toList
                            |> List.concat
                            |> List.map AstNode.Expression
                        if List.isEmpty expressions then None else Some expressions
                    | SynExpr.DotIndexedGet(e,es,_,_) -> Some((Expression e)::(List.map Expression es))
                    | SynExpr.LetOrUse(_,_,bs,e,_) ->  Some(List.append (List.map AstNode.Binding bs) [AstNode.Expression e])
                    | SynExpr.Match(_,e,cs,_,_) -> Some((AstNode.Expression e)::(List.map AstNode.MatchClause cs))
                    | SynExpr.Null(_) -> None
                    | SynExpr.Const(_,_) -> None
                    | SynExpr.Ident _ -> None
                    | SynExpr.App(_,_,e1,e2,_) -> Some([AstNode.Expression e1;AstNode.Expression e2])
                    | SynExpr.ArbitraryAfterError(_,_) -> None
                    | SynExpr.LongIdent(_,LongIdentWithDots(is,_),_,__) -> Some (List.map AstNode.Ident is)
                    | SynExpr.Lambda(_,_,p,e,_) -> Some([AstNode.Expression e;AstNode.SimplePatterns p])
                    | SynExpr.Sequential(_,_,e1,e2,_)
                    | SynExpr.IfThenElse(e1,e2,None,_,_,_,_) -> Some([AstNode.Expression e1; AstNode.Expression e2])
                    | SynExpr.IfThenElse(e1,e2,Some(e3),_,_,_,_) -> Some([AstNode.Expression e1; AstNode
.Expression e2; AstNode.Expression e3])
                    | SynExpr.ForEach(_,_,_,p,e1,e2,_) -> Some([AstNode.Expression e1; AstNode.Expression e2; AstNode.Pattern p])
                    | SynExpr.TryWith(e,_,cs,_,_,_,_) -> Some((AstNode.Expression e)::(List.map AstNode.MatchClause cs))
                    | SynExpr.FromParseError(e,_) -> Some([AstNode.Expression e])
                    | _ -> raise (new NotImplementedException("Add a new entry to pattern for Expression: " + (string e)))
            | Ident(i) -> None
            | MatchClause(Clause(p,we,e,_,_)) ->
                if Option.isSome we
                then Some([AstNode.Pattern p; AstNode.Expression we.Value; AstNode.Expression e])
                else Some([AstNode.Pattern p; AstNode.Expression e])
            //TODO: Update makeScopeTrees to know about types
            | TypeDefinition(SynTypeDefn.TypeDefn(_,r,ms,_)) ->
                Some((AstNode.TypeDefinitionRepresentation r)::(List.map AstNode.MemberDefinition ms))
            | TypeDefinitionRepresentation(r) ->
                match r with
                    | SynTypeDefnRepr.ObjectModel(_,ms,_) -> Some (List.map AstNode.MemberDefinition ms)
                    | SynTypeDefnRepr.Simple(_,_) -> None
            | MemberDefinition(m) ->
                match m with
                    | SynMemberDefn.Member(b,_) -> Some [AstNode.Binding b]
                    | SynMemberDefn.LetBindings(bs,_,_,_) -> Some(List.map AstNode.Binding bs)
                    | d -> raise (new NotImplementedException("Add a new entry to pattern for MemberDefinition: " + (string d)))
    let (|Range|_|) (node : AstNode) =
        match node with
            | File _ -> None
            | Pattern p -> Some(p.Range)
            | ModuleOrNamespace ns -> Some(ns.Range)
            | ModuleDeclaration m -> Some(m.Range)
            | Binding b -> Some(b.RangeOfBindingAndRhs)
            | Expression e -> Some(e.Range)
            | Ident i -> Some(i.idRange)
            | MatchClause c -> Some(c.Range)
            | SimplePatterns ps ->
                let SynSimplePats.SimplePats(_,r) | SynSimplePats.Typed(_,_,r) = ps
                Some r
            | SimplePattern p ->
                let SynSimplePat.Id(_,_,_,_,_,r) | SynSimplePat.Typed(_,_,r) | SynSimplePat.Attrib(_,_,r) = p
                Some r
            | TypeDefinition d -> Some(d.Range)
            | TypeDefinitionRepresentation r -> Some(r.Range)
            | MemberDefinition m -> Some(m.Range)

    // Utility functions to avoid having to match patterns to get children or range
    let GetChildren (node : AstNode) =
        match node with
            | Children(c) -> Some(c)
            | _ -> None

    let GetRange (node : AstNode) =
        match node with
            | Range(r) -> Some(r)
            | _ -> None

            
