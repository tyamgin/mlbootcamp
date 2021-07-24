FROM continuumio/anaconda3:latest

COPY *.py .

#COPY data /tmp/data

CMD mkdir -p /var
CMD mkdir -p /var/log
CMD python main.py --data-dir /tmp/data --res-path /var/log/result \
 && head -20 /var/log/result


# Для запуска:
# sudo docker build -t vkcup . && docker run -t vkcup