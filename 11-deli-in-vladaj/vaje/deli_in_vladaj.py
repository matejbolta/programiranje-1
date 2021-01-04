###############################################################################
# Želimo definirati pivotiranje na mestu za tabelo [a]. Ker bi želeli
# pivotirati zgolj dele tabele, se omejimo na del tabele, ki se nahaja med
# indeksoma [start] in [end] (vključujoč oba robova).
#
# Primer: za [start = 1] in [end = 7] tabelo
#
#     [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
#     [10, 0, 2, 4, 11, 5, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 4 pivot.)
#
# Sestavi funkcijo [pivot(a, start, end)], ki preuredi tabelo [a] tako, da bo
# element [ a[start] ] postal pivot za del tabele med indeksoma [start] in
# [end]. Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele [a].
#
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot(a, 1, 7)
#     3
#     >>> a
#     [10, 0, 2, 4, 11, 5, 17, 15, 18]
###############################################################################
'''
def pivot(a, start, end):
  p = a[start]
  left = start
  right = end
  # Pustimo si mesto za pivot na a[start]
  # Vse kar delamo delamo na left + 1
  while left != right:
    if a[left] < p: # se samo naprej premaknemo
      left += 1
    elif p < a[right]: # se samo noter premaknemo
      right -= 1
    else:
      a[left], a[right] = a[right], a[left]

  a[start], a[left-1] = a[left-1], a[start]
  return left # == right
'''

def pivot(a, start, end):
  # robni pogoj
  if start == end:
    return start

  p = a[start]
  prvi_vecji = start+1
  # invarianta: med [prvi_vecji] in [i] so samo elementi manjsi od [p]
  for i in range(start+1, end+1):
    if a[i] < p:
      a[prvi_vecji], a[i] = a[i], a[prvi_vecji]
      prvi_vecji += 1
  # vstavimo pivot na pravo mesto
  a[start], a[prvi_vecji - 1] = a[prvi_vecji - 1], a[start]
  return prvi_vecji - 1

###############################################################################
# V tabeli želimo poiskati vrednost k-tega elementa po velikosti.
#
# Primer: Če je
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po velikosti enak 5, ker so od njega manši elementi
#  2, 3 in 4. Pri tem štejemo indekse od 0 naprej, torej je "ničti" element 2.
#
# Sestavite funkcijo [kth_element(a, k)], ki v tabeli [a] poišče [k]-ti element
# po velikosti. Funkcija sme spremeniti tabelo [a]. Cilj naloge je, da jo
# rešite brez da v celoti uredite tabelo [a].
###############################################################################

def k_ti_po_vrsti(a, k, start, stop):
  pivot_i = pivot(a, start, stop)
  if pivot_i > k:
    return k_ti_po_vrsti(a, k, start, pivot_i - 1)
  if pivot_i < k:
    return k_ti_po_vrsti(a, k - pivot_i, pivot_i + 1, stop)
  else:
    return a[pivot_i]

def kth_element(a, k):
  if k >= len(a):
    return None
  return k_ti_po_vrsti(a, k, 0, len(a)-1)

###############################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja (quicksort).
#
# Napišite funkcijo [quicksort(a)], ki uredi tabelo [a] s pomočjo pivotiranja.
# Poskrbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: Definirajte pomožno funkcijo [quicksort_part(a, start, end)], ki
#        uredi zgolj del tabele [a].
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#     >>> quicksort(a)
#     >>> a
#     [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################
'''
def quicksort(a, start=0, stop=None):
  if stop is None:
    stop = len(a) - 1
  if start >= stop:
    return ##
  pivot_i = pivot(a, start, stop)
  quicksort(a, start, pivot_i)
  quicksort(a, pivot_i, stop)
'''

def quicksort(a):
  def quicky(start, end):
    if end - start <= 0:
      return ##
    pivot_i = pivot(a, start, end)
    quicky(start, pivot_i-1)
    quicky(pivot_i+1, end)
    return ##
  
  quicky(0, len(a)-1)
  return ##

###############################################################################
# Če imamo dve urejeni tabeli, potem urejeno združeno tabelo dobimo tako, da
# urejeni tabeli zlijemo. Pri zlivanju vsakič vzamemo manjšega od začetnih
# elementov obeh tabel. Zaradi učinkovitosti ne ustvarjamo nove tabele, ampak
# rezultat zapisujemo v že pripravljeno tabelo (ustrezne dolžine).
#
# Funkcija naj deluje v času O(n), kjer je n dolžina tarčne tabele.
#
# Sestavite funkcijo [merge(target, list_1, list_2)], ki v tabelo [target]
# zlije tabeli [list_1] in [list_2]. V primeru, da sta elementa v obeh tabelah
# enaka, naj bo prvi element iz prve tabele.
#
# Primer:
#
#     >>> list_1 = [1, 3, 5, 7, 10]
#     >>> list_2 = [1, 2, 3, 4, 5, 6, 7]
#     >>> target = [-1 for _ in range(len(list_1) + len(list_2))]
#     >>> merge(target, list_1, list_2)
#     >>> target
#     [1, 1, 2, 3, 3, 4, 5, 5, 6, 7, 7, 10]
#
###############################################################################
'''
def zlij(target, begin, end, list_1, list_2):
  i1 = 0
  i2 = 0
  while (i1 < len(list_1) and i2 < len(list_2)):
    if list_1[i1] <= list_2[i2]:
      target[begin + i1 + i2] = list_2[i1]
      i1 += 1
    else:
      target[begin + i1 + i2] = list_2[i2]
      i2 += 1

  # Če so pri kateremkoli že na koncu ni problema
  while i1 < len(list_1):
    target[begin + i1 + i2] = list_1[i1]
    i1 += 1
  while i2 < len(list_2):
    target[begin + i1 + i2] = list_2[i2]
    i2 += 1
'''

def merge(target, begin, end, list1, list2):
  i1 = 0
  i2 = 0
  for j in range(begin, end+1):
    i1_is_ok = i1 < len(list1)
    i2_is_ok = i2 < len(list2)

    if (i1_is_ok and not i2_is_ok) or (i1_is_ok and list1[i1] < list2[i2]):
      target[j] = list1[i1]
      i1 += 1
    elif i2_is_ok:
      target[j] = list2[i2]
      i2 += 1
    else:
      raise Exception("Lengths of lists are not sufficient")

###############################################################################
# Tabelo želimo urediti z zlivanjem (merge sort). Tabelo razdelimo na polovici,
# ju rekurzivno uredimo in nato zlijemo z uporabo funkcije [zlij].
#
# Namig: prazna tabela in tabela z enim samim elementom sta vedno urejeni.
#
# Napišite funkcijo [mergesort(a)], ki uredi tabelo [a] s pomočjo zlivanja. Za
# razliko od hitrega urejanja tu tabele lahko kopirate, zlivanje pa je potrebno
# narediti na mestu.
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#     >>> mergesort(a)
#     >>> a
#     [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################

# peace love unity  (!)
'''
def mergesort(a, begin=0, end=None):
  if end is None:
    end = len(a)
  if (begin < end - 1): # Treba je uredi, ker imamo seznam dolžine >= 2
    midpoint = (begin + end) // 2
    mergesort(a, begin, midpoint)
    mergesort(a, midpoint, end)
    
    prvi = a[begin:midpoint]
    drugi = a[midpoint:end]

    zlij(a, begin, end, prvi, drugi)

  else:
    return a
'''

def mergesort(a):
  if len(a) <= 1:
    return ##
  polovicka = len(a) // 2
  a1, a2 = a[:polovicka], a[polovicka:]
  mergesort(a1)
  mergesort(a2)
  merge(a, 0, len(a)-1, a1, a2)
  return ##
