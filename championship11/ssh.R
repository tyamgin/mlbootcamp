require(doParallel)

primary <- '192.168.0.100'

machineAddresses <- list(
  list(host=primary, user='tyamgin',
       ncore=1)
  #list(host='192.168.0.106', user='rdp',
  #     ncore=1)
)

spec <- lapply(machineAddresses,
               function(machine) {
                 rep(list(list(host=machine$host,
                               user=machine$user)),
                     machine$ncore)
               })
spec <- unlist(spec,recursive=FALSE)

print('starting...')
system('start G:\\Projects\\mlbootcamp\\championship11\\startNode.bat')
print('connecting...')

parallelCluster <- parallel::makeCluster(
  type='PSOCK', master=primary, spec=spec, rshcmd='ssh1 -i "C:\\Program Files\\OpenSSH-Win64\\ssh_host_rsa_key"', manual=T)
print('connected')

print(parallelCluster)