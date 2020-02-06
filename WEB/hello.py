def application(env, start_response):
    response = []

    response.append(env['QUERY_STRING'].replace('&', '\n'))

    status = '200 OK'
    headers = [
        ('Content-Type', 'text/plain')
    ]
    start_response(status, headers)
    return response
