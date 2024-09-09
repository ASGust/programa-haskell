module TrabalhoHaskell where

data Tarefa = Tarefa { descricao :: String } deriving (Eq)

adicionarTarefa :: [Tarefa] -> Tarefa -> [Tarefa]
adicionarTarefa lista_tarefa nova_tarefa = nova_tarefa : lista_tarefa

removerTarefa :: [Tarefa] -> Tarefa -> [Tarefa]
removerTarefa lista_tarefa retirar_tarefa = filter (/= retirar_tarefa) lista_tarefa

exibirTarefas :: [Tarefa] -> IO ()
exibirTarefas lista_tarefa = mapM_ (putStrLn . descricao) lista_tarefa

menu :: [Tarefa] -> IO ()
menu lista_tarefa = do
    putStrLn "\n------Menu de Opções------"
    putStrLn "----Escolha uma opção:----"
    putStrLn "1. Adicionar uma nova tarefa"
    putStrLn "2. Remova uma tarefa já existente"
    putStrLn "3. Mostrar lista de tarefas"
    putStrLn "4. Encerrar o programa..."
    escolha <- getLine
    case escolha of
        "1" -> adicionar lista_tarefa
        "2" -> remover lista_tarefa
        "3" -> exibir lista_tarefa
        "4" -> putStrLn "Encerrando..."
        _   -> do
            putStrLn "Opção inválida :("
            menu lista_tarefa

adicionar :: [Tarefa] -> IO ()
adicionar lista_tarefa = do
    putStrLn "Digite a tarefa que deseja inserir :"
    descricao <- getLine
    let nova_tarefa = Tarefa descricao
    let tarefa_atualizada = adicionarTarefa lista_tarefa nova_tarefa
    putStrLn "Tarefa adicionada com sucesso :)"
    menu tarefa_atualizada

remover :: [Tarefa] -> IO ()
remover lista_tarefa = do
    putStrLn "Digite a tarefa que quer remover(exatamente como foi escrita): "
    descricao <- getLine
    let retirar_tarefa = Tarefa descricao
    let tarefa_atualizada = removerTarefa lista_tarefa retirar_tarefa
    putStrLn "Tarefa removida com sucesso :)"
    menu tarefa_atualizada

exibir :: [Tarefa] -> IO ()
exibir lista_tarefa = do
    putStrLn "--Lista de tarefas--"
    exibirTarefas lista_tarefa
    menu lista_tarefa

main :: IO ()
main = do
    putStrLn "------Seja Bem-vindo------"
    menu []
