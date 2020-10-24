import csv
import os
import requests
import re

###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definirajte URL glavne strani bolhe za oglase z mačkami
cats_frontpage_url = 'http://www.bolha.com/zivali/male-zivali/macke/'
# mapa, v katero bomo shranili podatke
cat_directory = '02-zajem-podatkov/vaje/macke-podatki/'
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = 'index_macke.html'
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'macke.csv'


def download_url_to_string(url):
    """Funkcija kot argument sprejme url (niz) in poskusi vrniti vsebino te spletne
    strani kot niz. V primeru, da med izvajanje pride do napake vrne None.
    """
    try:
        # del kode, ki morda sproži napako
        page_content = requests.get(url)
    except requests.exceptions.ConnectionError as e:
        # koda, ki se izvede pri napaki
        # dovolj je če izpišemo opozorilo in prekinemo izvajanje funkcije
        print('Napaka pri povezovanju do:', url)
        print(e)
        return None
    
    # status_code nam pove, kakšen je bil odgovor (200, 404, 500, ...)
    if page_content.status_code == requests.codes.ok:
        return page_content.text
    else:
        # nadaljujemo s kodo če je prišlo do napake
        print('Napaka pri prenosu strani:', url)
        return None


def save_string_to_file(text, directory, filename):
    """Funkcija zapiše vrednost parametra "text" v novo ustvarjeno datoteko
    locirano v "directory"/"filename", ali povozi obstoječo. V primeru, da je
    niz "directory" prazen datoteko ustvari v trenutni mapi.
    """
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None


# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage(page, directory, filename):
    """Funkcija shrani vsebino spletne strani na naslovu "page" v datoteko
    "directory"/"filename"."""
    html = download_url_to_string(page)
    if html:
        save_string_to_file(html, directory, filename)
        return True
    return False


###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):
    """Funkcija vrne celotno vsebino datoteke "directory"/"filename" kot niz"""
    with open(os.path.join(directory, filename), 'r', encoding='utf-8') as f:
        return f.read()


# Definirajte funkcijo, ki sprejme niz, ki predstavlja vsebino spletne strani,
# in ga razdeli na dele, kjer vsak del predstavlja en oglas. To storite s
# pomočjo regularnih izrazov, ki označujejo začetek in konec posameznega
# oglasa. Funkcija naj vrne seznam nizov.


def page_to_ads(page_content):
    """Funkcija poišče posamezne oglase, ki se nahajajo v spletni strani in
    vrne njih seznam"""
    pattern = re.compile(
        r'<li class="EntityList-item EntityList-item--Regular'
        r'(.*?)</article>',
        re.DOTALL
    )
    return re.findall(pattern, page_content) # Vrne seznam oglasov


# Definirajte funkcijo, ki sprejme niz, ki predstavlja oglas, in izlušči
# podatke o imenu, lokaciji, datumu objave in ceni v oglasu.


def get_dict_from_ad_block(block):
    """Funkcija iz niza za posamezen oglasni blok izlušči podatke o imenu, ceni
    in opisu ter vrne slovar, ki vsebuje ustrezne podatke
    """
    # Za DOTALL:
    # Uporabili smo compile in na koncu dodali argument re.DOTALL
    # Lahko pa to dodamo pri re.ime_metode, npr. re.search(pattern, block, re.DOTALL)

    # (?P<ime_grupe>.*?) za poimenovanje grupe

    rx = re.compile(
        r'<h3.*>(?P<name>.*?)</a></h3>'
        r'.*?"pubdate">(?P<time>.*?)</time>'
        r'.*?<strong class="price price--hrk">\s*?(?P<price>\d*)&',
        re.DOTALL
    )

    data = re.search(rx, block)
    ad_dict = data.groupdict()

    # Ker nimajo vsi oglasi podatka o lokaciji, to rešimo z dodatnim vzorcem
    rloc = re.compile(r'Lokacija: </span>(?P<location>.*?)<br />')
    locdata = re.search(rloc, block)
    if locdata is not None:
        ad_dict['location'] = locdata.group('location')
    else:
        ad_dict['location'] = 'Unknown'

    return ad_dict


# Definirajte funkcijo, ki sprejme ime in lokacijo datoteke, ki vsebuje
# besedilo spletne strani, in vrne seznam slovarjev, ki vsebujejo podatke o
# vseh oglasih strani.


def ads_from_file(directory, filename):
    """Funkcija prebere podatke v datoteki "directory"/"filename" in jih
    pretvori (razčleni) v pripadajoč seznam slovarjev za vsak oglas posebej."""
    page = read_file_to_string(directory, filename)
    ad_blocks = page_to_ads(page)

    return [
        get_dict_from_ad_block(ad) for ad in ad_blocks
    ] # Vrne seznam slovarjev


###############################################################################
# Obdelane podatke želimo sedaj shraniti.
###############################################################################


# Pomožna funkcija, ki jo uporabimo v naslednji funkciji
def write_csv(fieldnames, rows, directory, filename):
    """
    Funkcija v csv datoteko podano s parametroma "directory"/"filename" zapiše
    vrednosti v parametru "rows" pripadajoče ključem podanim v "fieldnames"
    """
    # fieldnames je seznam kljucev
    # rows je seznam slovarjev
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            # row je tukaj posamezen slovar za en oglas
            writer.writerow(row)
    return None


# Definirajte funkcijo, ki sprejme neprazen seznam slovarjev, ki predstavljajo
# podatke iz oglasa mačke, in zapiše vse podatke v csv datoteko. Imena za
# stolpce [fieldnames] pridobite iz slovarjev.


def write_cat_ads_to_csv(ads, directory, filename):
    """Funkcija vse podatke iz parametra "ads" zapiše v csv datoteko podano s
    parametroma "directory"/"filename". Funkcija predpostavi, da so ključi vseh
    sloverjev parametra ads enaki in je seznam ads neprazen.

    """
    # Stavek assert preveri da zahteva velja
    # Če drži se program normalno izvaja, drugače pa sproži napako
    # Prednost je v tem, da ga lahko pod določenimi pogoji izklopimo v
    # produkcijskem okolju
    # Preveri da je slovar neprazen in da so ključi povsod enaki.
    assert ads and (all(j.keys() == ads[0].keys() for j in ads))
    write_csv(ads[0].keys(), ads, directory, filename)
    return None


# Celoten program poženemo v glavni funkciji

def main(redownload=True, reparse=True):
    """Funkcija izvede celoten del pridobivanja podatkov:
    1. Oglase prenese iz bolhe
    2. Lokalno html datoteko pretvori v lepšo predstavitev podatkov (seznam slovarjev)
    3. Podatke shrani v csv datoteko
    """
    # Najprej v lokalno html datoteko shranimo glavno stran
    save_frontpage(cats_frontpage_url, cat_directory, frontpage_filename)

    # Iz lokalne (html) datoteke preberemo podatke, najprej kot seznam blokov
    # Podatke prebermo v lepšo obliko (seznam slovarjev)
    # To vse je združeno v funkciji ads_from_file
    ads = ads_from_file(cat_directory, frontpage_filename)

    # Podatke shranimo v csv datoteko
    write_cat_ads_to_csv(ads, cat_directory, csv_filename)

    # Dodatno: S pomočjo parametrov funkcije main omogoči nadzor, ali se
    # celotna spletna stran ob vsakem zagon prenese (četudi že obstaja)
    # in enako za pretvorbo
    # To sta redownload in reparse


if __name__ == '__main__':
    # To povzroči, da se main() požene le, ko je pognana datoteka macke.py,
    # ne pa vsakic ko je klicana (npr pri import macke)
    main()
