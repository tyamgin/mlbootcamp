require(doParallel)

primary <- '192.168.0.100'

machineAddresses <- list(
  list(host=primary, user='tyamgin',
       ncore=4),
  list(host='192.168.0.106', user='rdp',
       ncore=4)
)

spec <- lapply(machineAddresses,
               function(machine) {
                 rep(list(list(host=machine$host,
                               user=machine$user)),
                     machine$ncore)
               })
spec <- unlist(spec,recursive=FALSE)

print('connecting...')

parallelCluster <- parallel::makeCluster(
  type='PSOCK', master=primary, spec=spec, rshcmd='ssh1 -i "C:\\Program Files\\OpenSSH-Win64\\ssh_host_rsa_key"', manual=F, homogeneous=T, rscript="lol")
print('connected')

registerDoParallel(parallelCluster)

print(foreach(i=1:8) %dopar% {unname(Sys.info()["nodename"])})

stopCluster(parallelCluster)
