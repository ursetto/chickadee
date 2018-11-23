## HEAD doesn't work properly (200 instead of 302)

HEAD doesn't work. It always returns 200 when a 302 should be returned. Example
    http://localhost:8389/cdoc?q=openssl

But GET works

    curl -v http://localhost:8389/cdoc?q=openssl

## GET sometimes doesn't work properly (200 instead of 404)
GET to illegal cdoc path with query string returns 200,

     curl -v 'http://localhost:8388/cdoc?q=9p1'

but GET with illegal doc path correctly returns 404.

     curl -v http://localhost:8388/doc/9p1

