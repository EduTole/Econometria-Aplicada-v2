import pandas
import numpy

# Codigos de ENAHO

# --------------------------------------------------------
# Modulo de Empleo
# --------------------------------------------------------

def function_r500(frame):
    
    
    # nivel de categoria ocupacional
    frame = frame.copy()
    
    # Codigo de persona y hogar
    frame['rcod_persona'] = frame['conglome']+frame['vivienda']+frame['hogar']+frame['codperso']
    frame['rcod_hogar']   = frame['conglome']+frame['vivienda']+frame['hogar']

    frame['r3'] = numpy.where(frame['ocu500']=='ocupado', 1,
                              numpy.where(frame['ocu500']=='desocupado abierto',2, 3))
    
    #frame['r3'] = frame['ocu500'].replace({1:1, 2:2, 3:3, 4:3})
    
    # Desempleo
    frame['rmu'] = numpy.where(frame['ocu500']=='desocupado abierto',1,0)
    #frame['rmu'] = numpy.where(frame['ocu500']==2,1,0)
   
    # variable edad
    frame['redad'] = frame['p208a']
    frame['redad'] = frame['redad'].astype('int32')
    frame['redadsq'] = frame['redad']**2
    
    # variables de ingresos laborales
    variables_income = ['i524a1', 'd529t', 'i530a', 'd536', 'i538a1', 'd540t', 'i541a', 'd543', 'd544t']
    base_income = frame[variables_income]
    #frame['r6'] = numpy.where(frame['ocu500']=="ocupado", base_income.sum(axis=1),0)
    frame['r6'] = numpy.where(frame['r3']==1, base_income.sum(axis=1),0)
    frame['r6'] = frame['r6'].astype('int32')
    frame['r6'] = frame['r6']/12
    frame['lnr6'] = numpy.where(frame['r6']>0,numpy.log(frame['r6']),0)
    
    # Variable de edad
    frame['rmujer'] = numpy.where(frame['p207']=='mujer', 1, 0)
    #frame['rmujer'] = numpy.where(frame['p207']==2, 1, 0)
    
    # Nivel educativo
    frame['rneduca'] = frame['p301a'].replace({"sin nivel": "inicial",
                                         "básica especial": "inicial",
                                         "inicial": "inicial",
                                         "primaria completa": "primaria",
                                         "primaria incompleta": "primaria",
                                         "secundaria completa": "secundaria",
                                         "secundaria incompleta": "secundaria",
                                         "superior no universitaria completa": "tecnica",
                                         "superior no universitaria incompleta": "tecnica",
                                         "superior universitaria completa": "Univ.",
                                         "superior universitaria incompleta": "Univ.",
                                         "maestria/doctorado": "Univ."})
    
    frame['rDpto'] = frame['ubigeo'].str[0:2]
    frame['rDpto'] = frame['rDpto'].replace({"01": "Amazonas","02": "Ancash","03": "Apurimac","04":"Arequipa","05": "Ayacucho",
                                           "06": "Cajamarca","07": "Callao","08": "Cusco","09": "Huancavelica","10":"Huanuco",
                                           "11": "Ica","12":"Junin", "13": "La Libertad","14": "Lambayeque","15":"Lima",
                                           "16": "Loreto","17":"Madre de Dios","18": "Moquegua","19":"Pasco","20":"Piura",
                                           "21": "Puno","22":"San Martin","23":"Tacna","24":"Tumbes","25":"Ucayali"
                                            })

    # Estado civil
    frame['pareja'] = frame['p209'].replace({"casado(a)": "pareja", 
                                              "conviviente": "pareja",
                                              "viudo(a)": "no-pareja",
                                              "divorciado(a)": "no-pareja",
                                              "separado(a)": "no-pareja",
                                              "soltero(a)": "no-pareja",
                                             })
    frame['rpareja'] = numpy.where(frame['pareja']=='pareja',1,0)
    
    #frame.loc[(frame['p209']=='casado(a)') & (frame['p209']=='conviviente'),'rpareja']  = (frame['rpareja'] + 0)
    #frame.loc[(frame['p209']==1) & (frame['p209']==2),'rpareja']  = 1
    

    return frame

# --------------------------------------------------------
# Modulo de Educacion 
# --------------------------------------------------------
def function_r300(frame):
    
    """
    El comando variable_reduca, genera la variable de reduca, el cual permitira la variable cuantiativa de los
    años de educacion de la persona, considerando las varuables: p301a, p301b, p301c 
    """
    frame = frame.copy()

     # Codigo de persona y hogar
    frame['rcod_persona'] = frame['conglome']+frame['vivienda']+frame['hogar']+frame['codperso']
    frame['rcod_hogar']   = frame['conglome']+frame['vivienda']+frame['hogar']
   
    
    frame['p301a'] = pandas.to_numeric(frame['p301a'])
    frame['p301b'] = pandas.to_numeric(frame['p301b'])
    frame['p301c'] = pandas.to_numeric(frame['p301c'])
    zeros = frame['p301c'].min()

    frame['reduca'] = frame['p301b']
    #frame['reduca'] = frame['reduca'].astype('int32')
    
    frame.loc[(frame['p301a']>=1) & (frame['p301a']<=4),'reduca']  = (frame['reduca'] + 0)
    frame.loc[(frame['p301a']>=5) & (frame['p301a']<=6),'reduca']  = (frame['reduca'] + 6)
    frame.loc[(frame['p301a']>=7) & (frame['p301a']<=10),'reduca'] = (frame['reduca'] + 11)
    frame.loc[(frame['p301a']==11),'reduca'] = (frame['reduca'] + 16)
    
    return frame


def funcion_rfiltros(frame):
    
    frame = frame.copy()
    
    # Filtro de edad
    frame = frame[(frame.redad>17) & (frame.redad<70)]
    
    # Filtro de nivel educativo y desempleo , diferente de missing values
    frame = frame.dropna(subset=['rmu', 'rneduca','reduca'])

    return frame


def funcion_r3(frame, target, pregunta):
    
    frame = frame.copy()
    frame[target] = numpy.where(frame[pregunta]=='ocupado', 1, 
                               numpy.where(frame[pregunta]=='desocupado abierto',2, 3))
    
    return frame

def funcion_redad(frame, target, pregunta):
    frame = frame.copy()    
    frame[target] = frame[pregunta]
    
    return frame

def funcion_r6(frame, target, pregunta):
    frame = frame.copy()
    variables_income = ['i524a1', 'd529t', 'i530a', 'd536', 'i538a1', 'd540t', 'i541a', 'd543', 'd544t']
    base_income = frame[variables_income]
    frame[target] = numpy.where(frame[pregunta]=="ocupado", base_income.sum(axis=1),0)
    frame[target] = frame[target]/12
    return frame

def funcion_genero(frame, target, pregunta):
    #frame = frame.copy()
    frame[target] = numpy.where(frame[pregunta]=='mujer', 1, 0)
    
    return frame

def funcion_r2(frame, target, pregunta):
    frame = frame.copy()
    def nivel_condicion(x):
        if "sin nivel" in x:
            return "sin_nivel"
        if "inicial" in x:
            return "sin_nivel"
        elif "primaria completa" in x:
            return "primaria"
        elif "primaria incompleta" in x:
            return "primaria"
        elif "secundaria completa" in x:
            return "secundaria"
        elif "secundaria incompleta" in x:
            return "secundaria" 
        elif "básica especial" in x:
            return "secundaria"         
        elif "superior no universitaria completa" in x:
            return "tecnica"
        elif "superior no universitaria incompleta" in x:
            return "tecnica"          
        elif "superior universitaria completa"  in x:
            return "superior" 
        elif "superior universitaria incompleta"  in x:
            return "superior" 
        elif "maestria/doctorado"  in x:
            return "superior"
        #elif ("superior universitaria completa" or "superior universitaria incompleta" or "maestria/doctorado") in x:
        #    return "superior"  
        else:
            return "otro_nivel"
        
    
    frame[target] = frame[pregunta].apply(nivel_condicion)
    
    # departamento
    frame['rDpto'] = frame['ubigeo'].str[0:2]
    frame['rDpto'] = frame['rDpto'].replace({"01": "Amazonas","02": "Ancash","03": "Apurimac","04":"Arequipa","05": "Ayacucho",
                                           "06": "Cajamarca","07": "Callao","08": "Cusco","09": "Huancavelica","10":"Huanuco",
                                           "11": "Ica","12":"Junin", "13": "La Libertad","14": "Lambayeque","15":"Lima",
                                           "16": "Loreto","17":"Madre de Dios","18": "Moquegua","19":"Pasco","20":"Piura",
                                           "21": "Puno","22":"San Martin","23":"Tacna","24":"Tumbes","25":"Ucayali"
                                          })    
    
    
    
    return frame

def function_rsumaria(frame):
    
    # nivel de categoria ocupacional
    frame = frame.copy()
    
    # Codigo de persona y hogar
    frame['rcod_hogar']   = frame['conglome']+frame['vivienda']+frame['hogar']
    
    # Pobreza
    frame['rpobre'] = numpy.where(frame['pobreza']=="no pobre",0,1)
    
    # Miembros del hogar
    frame['rmiembro'] = frame['mieperho']
    frame['rmiembro'] = frame['rmiembro'].astype('int32')
    
    # rgasto del hogar
    frame['gashog2d'] = frame['gashog2d'].astype('int32')
    frame['rgasto'] = frame['gashog2d'] / frame['mieperho'] / 12

    # Electricidad
    #frame['relectricidad'] = numpy.where(frame[variable]=="electricidad",1,0)
    
    return frame

def  funcion_rpobre(frame , target, variable):
    frame = frame.copy()
    frame[target] = numpy.where(frame[variable]=="no pobre",0,1)
    
    return frame

def  funcion_rmiembro(frame , target, variable):
    frame = frame.copy()
    frame[target] = frame[variable]
    
    return frame

def  funcion_rgasto(frame , target, variable):
    frame = frame.copy()
    frame[target] = frame[variable] / frame['mieperho'] / 12
    
    return frame

def funcion_relectricidad(frame , target, variable):
    
    frame = frame.copy()
    frame[target] = numpy.where(frame[variable]=="electricidad",1,0)
    
    return frame
