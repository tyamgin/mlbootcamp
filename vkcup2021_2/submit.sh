echo "Starting.." \
  && sudo docker build -t vkcup . \
  && docker tag vkcup stor.highloadcup.ru/vkcup21_age/yellow_jaguar \
  && docker push stor.highloadcup.ru/vkcup21_age/yellow_jaguar \
  && echo "Done"