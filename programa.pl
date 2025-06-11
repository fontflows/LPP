/*
 ======================================================
 SISTEMA DE SIMULACAO DE EVOLUCAO EM AMBIENTES VIRTUAIS
 Linguagens e paradigmas de programacao - Trabalho1 - Prolog
 Autores: 
 Mikhael Lazarev Nogueira Barbosa - NUSP: 15480495
 Francisco Eduardo Fontenele       - NUSP: 15452569
 Daniel Natan dos Santos Brito     - NUSP: 15446908
 Tema 14: Simulacao de Evolucao em Ambientes Virtuais
 ======================================================
*/


/*
Ambiente: ambiente(Tamanho, UltimoIdRecurso, UltimoIdOrganismo, Rodada)
Organismo: organismo(Id, Genes, Posicao, Energia)
Genes: genes(Forca, Eficiencia, Sexo) - Sexo: 1=Masculino, 0=Feminino
Recurso: recurso(Id, Posicao)
Posicao: pos(X, Y)
*/

:- dynamic ambiente/4.
:- dynamic organismo/4.
:- dynamic recurso/2.

main :-
    retractall(ambiente(_, _, _, _)),
    retractall(organismo(_, _, _, _)),
    retractall(recurso(_, _)),
    assert(ambiente(0, 0, 0, 0)),
    menu_principal.

menu_principal :-
    nl,
    write('1 - Inicializar ambiente'), nl,
    write('2 - Adicionar Organismo'), nl,
    write('3 - Adicionar Recursos'), nl,
    write('4 - Executar Simulacao'), nl,
    write('5 - Ver Estatisticas'), nl,
    write('6 - Resetar Simulacao'), nl,
    write('7 - Sair'), nl, nl,
    write('Escolha uma opcao: '),
    read(Escolha),
    processar_escolha(Escolha).

processar_escolha(1) :-
    inicializar_ambiente,
    menu_principal.

processar_escolha(2) :-
    adicionar_organismos,
    menu_principal.

processar_escolha(3) :-
    adicionar_recursos,
    menu_principal.

processar_escolha(4) :-
    executar_simulacao,
    menu_principal.

processar_escolha(5) :-
    estatisticas,
    menu_principal.

processar_escolha(6) :-
    resetar,
    menu_principal.

processar_escolha(7) :-
    write('Saindo do programa...'), nl.

processar_escolha(_) :-
    write('Opcao invalida!'), nl,
    menu_principal.

inicializar_ambiente :-
    write('Qual o tamanho? (ex: 100): '),
    read(Tamanho),
    retractall(ambiente(_, _, _, _)),
    assert(ambiente(Tamanho, 0, 0, 0)),
    write('Ambiente inicializado com sucesso!'), nl.

adicionar_organismos :-
    ambiente(TamAmb, _, _, _),
    (TamAmb =:= 0 ->
        write('Erro: Ambiente nao foi inicializado!'), nl
    ;
        write('Qual a quantidade? '),
        read(Quantidade),
        adicionar_organismos_aux(Quantidade),
        format('~w organismos adicionados!~n', [Quantidade])
    ).

adicionar_organismos_aux(0).
adicionar_organismos_aux(N) :-
    N > 0,
    gerar_organismo,
    N1 is N - 1,
    adicionar_organismos_aux(N1).

gerar_organismo :-
    ambiente(Tamanho, IdRec, IdOrg, Rodada),
    NovoId is IdOrg + 1,
    random(0, Tamanho, X),
    random(0, Tamanho, Y),
    gerar_genes(Genes),
    assert(organismo(NovoId, Genes, pos(X, Y), 30)),
    retractall(ambiente(_, _, _, _)),
    assert(ambiente(Tamanho, IdRec, NovoId, Rodada)).

gerar_genes(genes(Forca, Eficiencia, Sexo)) :-
    random(0, 100, Forca),
    random(0, 100, Eficiencia),
    random(0, 2, Sexo).

adicionar_recursos :-
    ambiente(Tamanho, _, _, _),
    (Tamanho =:= 0 ->
        write('Erro: Ambiente nao foi inicializado!'), nl
    ;
        write('Qual a quantidade? '),
        read(Quantidade),
        adicionar_recursos_aux(Quantidade),
        format('~w recursos adicionados!~n', [Quantidade])
    ).

adicionar_recursos_aux(0).
adicionar_recursos_aux(N) :-
    N > 0,
    ambiente(Tamanho, IdRec, IdOrg, Rodada),
    NovoIdRec is IdRec + 1,
    random(0, Tamanho, X),
    random(0, Tamanho, Y),
    assert(recurso(NovoIdRec, pos(X, Y))),
    retractall(ambiente(_, _, _, _)),
    assert(ambiente(Tamanho, NovoIdRec, IdOrg, Rodada)),
    N1 is N - 1,
    adicionar_recursos_aux(N1).

resetar :-
    retractall(ambiente(_, _, _, _)),
    retractall(organismo(_, _, _, _)),
    retractall(recurso(_, _)),
    assert(ambiente(0, 0, 0, 0)),
    write('Simulacao resetada!'), nl.

estatisticas :-
    nl,
    write('=== ESTATISTICAS ==='), nl, nl,
    ambiente(Tamanho, _, _, _),
    findall(_, organismo(_, _, _, _), ListaOrgs),
    length(ListaOrgs, NumOrgs),
    findall(_, recurso(_, _), ListaRecs),
    length(ListaRecs, NumRecs),
    write('Ambiente:'), nl,
    format('  Tamanho: ~w~n~n', [Tamanho]),
    format('Organismos (~w total):~n', [NumOrgs]),
    mostrar_organismos,
    nl,
    format('Recursos (~w total):~n', [NumRecs]),
    mostrar_recursos,
    nl.

mostrar_organismos :-
    findall(organismo(Id, genes(F, E, S), pos(X, Y), Energia), 
            organismo(Id, genes(F, E, S), pos(X, Y), Energia), 
            Organismos),
    sort(Organismos, OrganismosOrdenados),
    mostrar_lista_organismos(OrganismosOrdenados).

mostrar_lista_organismos([]).
mostrar_lista_organismos([organismo(Id, genes(F, E, S), pos(X, Y), Energia)|Resto]) :-
    format('ID: ~w  Genes: (~w ~w ~w) Posicao: (~w ~w) Energia: ~w~n', [Id, F, E, S, X, Y, Energia]),
    mostrar_lista_organismos(Resto).

mostrar_recursos :-
    findall(recurso(Id, pos(X, Y)), 
            recurso(Id, pos(X, Y)), 
            Recursos),
    sort(Recursos, RecursosOrdenados),
    mostrar_lista_recursos(RecursosOrdenados).

mostrar_lista_recursos([]).
mostrar_lista_recursos([recurso(Id, pos(X, Y))|Resto]) :-
    format('ID: ~w  Posicao: (~w, ~w)~n', [Id, X, Y]),
    mostrar_lista_recursos(Resto).

executar_simulacao :-
    ambiente(Tamanho, _, _, _),
    (Tamanho =:= 0 ->
        write('Erro: Ambiente nao foi inicializado!'), nl
    ;
        nl,
        write('=== ESTADO INICIAL ==='), nl,
        estatisticas_simples,
        write('Digite o numero de rodadas: '),
        read(Rodadas),
        nl,
        executar_rodadas(1, Rodadas)
    ).

estatisticas_simples :-
    findall(_, organismo(_, _, _, _), ListaOrgs),
    length(ListaOrgs, NumOrgs),
    findall(_, recurso(_, _), ListaRecs),
    length(ListaRecs, NumRecs),
    format('Organismos vivos: ~w, Recursos restantes: ~w~n', [NumOrgs, NumRecs]).

executar_rodadas(Atual, Total) :-
    Atual > Total,
    write('=== SIMULACAO FINALIZADA ==='), nl,
    estatisticas_simples.
executar_rodadas(RodadaAtual, Total) :-
    RodadaAtual =< Total,
    format('~n=== Rodada ~w ===~n', [RodadaAtual]),
    processar_organismos,
    adicionar_recursos_rodada,
    remover_organismos_mortos,
    contar_organismos_vivos(OrganismosVivos),
    contar_recursos_restantes(RecursosRestantes),
    format('Fim da rodada ~w. Organismos vivos: ~w, Recursos restantes: ~w~n', [RodadaAtual, OrganismosVivos, RecursosRestantes]),
    ProximaRodada is RodadaAtual + 1,
    executar_rodadas(ProximaRodada, Total).

contar_organismos_vivos(Contador) :-
    findall(_, organismo(_, _, _, _), Lista),
    length(Lista, Contador).

contar_recursos_restantes(Contador) :-
    findall(_, recurso(_, _), Lista),
    length(Lista, Contador).

processar_organismos :-
    findall(organismo(Id, Genes, Pos, Energia), organismo(Id, Genes, Pos, Energia), Organismos),
    processar_cada_organismo(Organismos).

processar_cada_organismo([]).
processar_cada_organismo([organismo(Id, Genes, Pos, Energia)|Resto]) :-
    (organismo(Id, _, _, _) ->
        (decidir_acao(Energia, Acao),
         executar_acao(Id, Genes, Pos, Energia, Acao))
    ; true),
    processar_cada_organismo(Resto).

decidir_acao(Energia, 1) :-
    Energia =< 10, !.
decidir_acao(_, Acao) :-
    random(0, 2, Acao).

executar_acao(Id, genes(Forca, Eficiencia, Sexo), Pos, Energia, 1) :-
    calcular_gasto_energia(Eficiencia, GastoEnergia),
    NovaEnergia is Energia - GastoEnergia,
    buscar_recurso_mais_proximo(Pos, RecursoId, DistanciaRecurso),
    (RecursoId \= -1 ->
        competir_por_recurso(Id, Forca, RecursoId, NovaEnergia, DistanciaRecurso)
    ;
        atualizar_organismo(Id, genes(Forca, Eficiencia, Sexo), Pos, NovaEnergia)
    ).

executar_acao(Id, genes(Forca, Eficiencia, 1), Pos, Energia, 0) :-
    buscar_femea_para_reproducao(Id, genes(Forca, Eficiencia, 1), Pos, Energia).

executar_acao(Id, genes(Forca, Eficiencia, 0), Pos, Energia, 0) :-
    atualizar_organismo(Id, genes(Forca, Eficiencia, 0), Pos, Energia).

calcular_gasto_energia(Eficiencia, 10) :- Eficiencia =< 40, !.
calcular_gasto_energia(Eficiencia, 7) :- Eficiencia =< 80, !.
calcular_gasto_energia(_, 5).

buscar_recurso_mais_proximo(pos(X, Y), RecursoId, MenorDistancia) :-
    findall(dist(Id, Dist), (recurso(Id, pos(Rx, Ry)), 
                            Dist is sqrt((X-Rx)^2 + (Y-Ry)^2)), 
            Distancias),
    (Distancias = [] ->
        RecursoId = -1,
        MenorDistancia = 999999
    ;
        sort(2, @=<, Distancias, [dist(RecursoId, MenorDistancia)|_])
    ).

competir_por_recurso(IdOrg, Forca, RecursoId, Energia, DistanciaRecurso) :-
    findall(comp(Id, F, Dist), 
            (organismo(Id, genes(F, _, _), PosComp, _), 
             Id \= IdOrg,
             buscar_recurso_mais_proximo(PosComp, RecursoComp, Dist),
             RecursoComp = RecursoId), 
            Competidores),
    CompetidoresComDistancia = [comp(IdOrg, Forca, DistanciaRecurso)|Competidores],
    encontrar_mais_forte(CompetidoresComDistancia, Vencedor),
    (Vencedor = IdOrg ->
        consumir_recurso(IdOrg, RecursoId, Energia)
    ;
        (organismo(IdOrg, Genes, Pos, _) ->
            atualizar_organismo(IdOrg, Genes, Pos, Energia)
        ; true)
    ).

encontrar_mais_forte([comp(Id, Forca, _)], Id) :- !.
encontrar_mais_forte([comp(Id1, F1, _), comp(Id2, F2, _)|Resto], Vencedor) :-
    (F1 >= F2 ->
        encontrar_mais_forte([comp(Id1, F1, 0)|Resto], Vencedor)
    ;
        encontrar_mais_forte([comp(Id2, F2, 0)|Resto], Vencedor)
    ).

consumir_recurso(IdOrg, RecursoId, Energia) :-
    retract(recurso(RecursoId, _)),
    NovaEnergia is Energia + 8,
    (organismo(IdOrg, Genes, Pos, _) ->
        atualizar_organismo(IdOrg, Genes, Pos, NovaEnergia)
    ; true).

buscar_femea_para_reproducao(IdMacho, genes(Forca, Eficiencia, 1), pos(X, Y), Energia) :-
    findall(dist(Id, Dist), (organismo(Id, genes(_, _, 0), pos(Fx, Fy), FemEnergia), 
                            FemEnergia > 10,
                            Id \= IdMacho,
                            Dist is sqrt((X-Fx)^2 + (Y-Fy)^2)), 
            FemeasDisponiveis),
    (FemeasDisponiveis = [] ->
        atualizar_organismo(IdMacho, genes(Forca, Eficiencia, 1), pos(X, Y), Energia)
    ;
        sort(2, @=<, FemeasDisponiveis, [dist(IdFemea, _)|_]),
        reproduzir_organismos(IdMacho, IdFemea)
    ).

reproduzir_organismos(IdMacho, IdFemea) :-
    organismo(IdMacho, GenesMacho, PosMacho, EnergiaMacho),
    organismo(IdFemea, GenesFemea, PosFemea, EnergiaFemea),
    NovaEnergiaMacho is EnergiaMacho - 10,
    NovaEnergiaFemea is EnergiaFemea - 10,
    gerar_filho(GenesMacho, GenesFemea, PosMacho, PosFemea),
    atualizar_organismo(IdMacho, GenesMacho, PosMacho, NovaEnergiaMacho),
    atualizar_organismo(IdFemea, GenesFemea, PosFemea, NovaEnergiaFemea).

gerar_filho(genes(F1, E1, _), genes(F2, E2, _), pos(X1, Y1), pos(X2, Y2)) :-
    ambiente(Tamanho, IdRec, IdOrg, Rodada),
    NovoId is IdOrg + 1,
    ForcaFilho is (F1 + F2) // 2,
    EficienciaFilho is (E1 + E2) // 2,
    random(0, 2, SexoFilho),
    aplicar_mutacao(ForcaFilho, EficienciaFilho, SexoFilho, ForcaFinal, EficienciaFinal, SexoFinal),
    PosXFilho is (X1 + X2) / 2,
    PosYFilho is (Y1 + Y2) / 2,
    assert(organismo(NovoId, genes(ForcaFinal, EficienciaFinal, SexoFinal), pos(PosXFilho, PosYFilho), 30)),
    retractall(ambiente(_, _, _, _)),
    assert(ambiente(Tamanho, IdRec, NovoId, Rodada)),
    format('Novo organismo nasceu: ID ~w~n', [NovoId]).

aplicar_mutacao(Forca, Eficiencia, Sexo, ForcaFinal, EficienciaFinal, SexoFinal) :-
    random(0, 10, ChanceMutacao),
    (ChanceMutacao < 3 ->
        random(0, 2, GeneMutacao),
        random(0, 2, SinalMutacao),
        (SinalMutacao =:= 0 -> Multiplicador = -1; Multiplicador = 1),
        Quantidade is 8 * Multiplicador,
        aplicar_mutacao_gene(GeneMutacao, Quantidade, Forca, Eficiencia, Sexo, ForcaFinal, EficienciaFinal, SexoFinal)
    ;
        ForcaFinal = Forca,
        EficienciaFinal = Eficiencia,
        SexoFinal = Sexo
    ).

aplicar_mutacao_gene(0, Quantidade, Forca, Eficiencia, Sexo, ForcaFinal, Eficiencia, Sexo) :-
    ForcaTemp is Forca + Quantidade,
    (ForcaTemp < 0 -> ForcaFinal = 0; 
     (ForcaTemp > 100 -> ForcaFinal = 100; ForcaFinal = ForcaTemp)).

aplicar_mutacao_gene(1, Quantidade, ForcaOriginal, Eficiencia, Sexo, ForcaOriginal, EficienciaFinal, Sexo) :-
    EficienciaTemp is Eficiencia + Quantidade,
    (EficienciaTemp < 0 -> EficienciaFinal = 0;
     (EficienciaTemp > 100 -> EficienciaFinal = 100; EficienciaFinal = EficienciaTemp)).

atualizar_organismo(Id, Genes, Pos, NovaEnergia) :-
    (organismo(Id, _, _, _) ->
        (retract(organismo(Id, _, _, _)),
         assert(organismo(Id, Genes, Pos, NovaEnergia)))
    ; true).

adicionar_recursos_rodada :-
    ambiente(Tamanho, IdRec, IdOrg, Rodada),
    Quantidade is max(1, Tamanho // 10),
    adicionar_recursos_rodada_aux(Quantidade, IdRec, NovoIdRec),
    retractall(ambiente(_, _, _, _)),
    assert(ambiente(Tamanho, NovoIdRec, IdOrg, Rodada)).

adicionar_recursos_rodada_aux(0, IdRec, IdRec).
adicionar_recursos_rodada_aux(N, IdRec, NovoIdRec) :-
    N > 0,
    ambiente(Tamanho, _, _, _),
    IdAtual is IdRec + 1,
    random(0, Tamanho, X),
    random(0, Tamanho, Y),
    assert(recurso(IdAtual, pos(X, Y))),
    N1 is N - 1,
    adicionar_recursos_rodada_aux(N1, IdAtual, NovoIdRec).

remover_organismos_mortos :-
    findall(Id, (organismo(Id, _, _, Energia), Energia =< 0), MortosIds),
    remover_organismos_lista(MortosIds).

remover_organismos_lista([]).
remover_organismos_lista([Id|Resto]) :-
    retract(organismo(Id, _, _, _)),
    format('Organismo ~w morreu~n', [Id]),
    remover_organismos_lista(Resto).