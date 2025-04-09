def enoe_load(año, n_trim, tabla):
    url_gn_enoe = "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos"
    last_two_digits = str(año)[-2:]
    if (2005 <= año <= 2019) and (n_trim <= 4):
        list_url = [f"{url_gn_enoe}/{año}trim{n_trim}_csv.zip",
                    f"{tabla}t{n_trim}{last_two_digits}.csv"]
    elif año == 2020 and n_trim == 1:
        list_url = [
            f"{url_gn_enoe}/{año}trim{n_trim}_csv.zip",
            f"ENOE_{tabla}T{n_trim}{last_two_digits}.csv"]
    elif año == 2020 and n_trim == 2:
        etoe_link = "https://www.inegi.org.mx/contenidos/investigacion/etoe"
        raise Exception(f"Para este trimestre no existe versión ENOE. Prueba con ETOE 2020: {etoe_link}")

    elif (año == 2020 and n_trim == 3) or ((2021 <= año <= 2022) and (1 <= n_trim <= 4)):
        list_url = [
            f"{url_gn_enoe}/enoe_n_{año}_trim{n_trim}_csv.zip",
            f"ENOEN_{tabla}T{n_trim}{last_two_digits}.csv"]
    elif año == 2020 and n_trim == 4:
        list_url = [
            f"{url_gn_enoe}/enoe_n_{año}_trim{n_trim}_csv.zip",
            f"enoen_{tabla}T{n_trim}{last_two_digits}.csv"]
    elif año >= 2023 and n_trim >= 1:
        list_url = [
            f"{url_gn_enoe}/enoe_{año}_trim{n_trim}_csv.zip",
            f"ENOE_{tabla}T{n_trim}{last_two_digits}.csv"]
    else:
        print("URL incorrecto")
        return None
    #URLS enoe y tabla
    url_data = list_url[0]
    modulo = list_url[1]
    #Ejecutar consulta
    request_data = requests.get(url_data).content
    #Guardo en zip file
    zip_form = ZipFile(BytesIO(request_data))
    #Cargamos datos
    with zip_form.open(modulo) as file:
         df_enoe = pd.read_csv(file, encoding='latin-1', engine="pyarrow") 

    return df_enoe
