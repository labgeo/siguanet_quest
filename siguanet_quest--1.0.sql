-- siguanet_quest 1.0 extension script file
-- complain if script is sourced in psql, rather than via CREATE EXTENSION
\echo Use "CREATE EXTENSION siguanet-quest" to load this file. \quit

SET check_function_bodies TO false;

CREATE FUNCTION quest_edificio_densidad(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_edificio_superficieestanciasocupadas(ARRAY[4,5,7,8,9,16], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE actividad = ANY (ARRAY[4,5,7,8,9,16])
										       AND substring(codigo from 1 for 4) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_edificio_densidad(character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en un edificio de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_edificio_densidad(''0037'');
- select quest_edificio_densidad(''0037'');';

CREATE FUNCTION quest_edificio_densidad(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_edificio_superficieestanciasocupadas(ARRAY[4,5,7,8,9,16], upper($1), upper($2)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE actividad = ANY (ARRAY[4,5,7,8,9,16])
						     AND coddpto = upper($1)
						     AND substring(codigo FROM 1 FOR 4) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_edificio_densidad(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en un departamento SIGUANET y en un edificio de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_edificio_densidad(''B101'', ''0037'');
- select quest_edificio_densidad(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_densidad(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_edificio_superficieestanciasocupadas($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM todaspersonas WHERE codigo IN (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 4) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_densidad(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en las estancias de una determinada actividad SIGUANET para un edificio. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_edificio_densidad(4, ''0037'');
- select quest_edificio_densidad(8, ''0037'');';

CREATE FUNCTION quest_edificio_densidad(tipo character varying, denominacion character varying, edificio character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  SELECT quest_edificio_superficieestanciasocupadas(tipo, denominacion, edificio) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM todaspersonas WHERE codigo IN
            (SELECT codigo FROM quest_estancias  WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
          '   AND substring(codigo FROM 1 FOR 4) = ' || quote_literal(edificio) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;   
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_densidad(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad, en un edificio. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_edificio_densidad(''crue'', ''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_densidadbecarios(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_edificio_superficieestanciasocupadasbec(ARRAY[4,5,7], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM becarios)
										       AND actividad = ANY (ARRAY[4,5,7])
										       AND substring(codigo from 1 for 4) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_edificio_densidadbecarios(character varying) IS 'Obtiene los m2 de espacio de trabajo por becario en un edificio de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_edificio_densidadbecarios(''0037'');
- select quest_edificio_densidadbecarios(''0037'');';

CREATE FUNCTION quest_edificio_densidadbecarios(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_edificio_superficieestanciasocupadasbec(ARRAY[4,5,7], $1, $2) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM becarios)
						     AND actividad = ANY (ARRAY[4,5,7])
						     AND coddpto = upper($1)
						     AND substring(codigo FROM 1 FOR 4) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_edificio_densidadbecarios(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por becarios en un departamento SIGUANET y en un edificio de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_edificio_densidadbecarios(''B101'', ''0037'');
- select quest_edificio_densidadbecarios(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_densidadbecarios(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_edificio_superficieestanciasocupadasbec($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM becarios WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM becarios)
                                           AND actividad = $1
                                           AND substring(codigo from 1 for 4) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_densidadbecarios(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por becario en las estancias de una determinada actividad SIGUANET para un edificio. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_edificio_densidadbecarios(4, ''0018'');
- select quest_edificio_densidadbecarios(8, ''0018'');';

CREATE FUNCTION quest_edificio_densidadbecarios(tipo character varying, denominacion character varying, edificio character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_edificio_superficieestanciasocupadasbec(actlist, edificio) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM becarios WHERE codigo IN
            (SELECT codigo FROM quest_estancias  
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 4) = ' || quote_literal(edificio) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_densidadbecarios(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene los m2 de espacio de trabajo por BECARIO en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un edificio. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_edificio_densidadbecarios(''crue'', ''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_densidadexternos(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_edificio_superficieestanciasocupadasext(ARRAY[7,8], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalexternos)
										       AND actividad = ANY (ARRAY[7,8])
										       AND substring(codigo from 1 for 4) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_edificio_densidadexternos(character varying) IS 'Obtiene los m2 de espacio de trabajo por externo en un edificio de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_edificio_densidadexternos(''0037'');
- select quest_edificio_densidadexternos(''0037'');';

CREATE FUNCTION quest_edificio_densidadexternos(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_edificio_superficieestanciasocupadasext(ARRAY[7,8], $1, $2) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM personalexternos)
						     AND actividad = ANY (ARRAY[7,8])
						     AND coddpto = upper($1)
						     AND substring(codigo FROM 1 FOR 4) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_edificio_densidadexternos(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por externos en un departamento SIGUANET y en un edificio de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_edificio_densidadexternos(''B101'', ''0037'');
- select quest_edificio_densidadexternos(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_densidadexternos(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_edificio_superficieestanciasocupadasext($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM personalexternos WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalexternos)
                                           AND actividad = $1
                                           AND substring(codigo from 1 for 4) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_densidadexternos(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por externo en las estancias de una determinada actividad SIGUANET para un edificio. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_edificio_densidadexternos(4, ''0018'');
- select quest_edificio_densidadexternos(8, ''0018'');';

CREATE FUNCTION quest_edificio_densidadexternos(tipo character varying, denominacion character varying, edificio character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_edificio_superficieestanciasocupadasext(actlist, edificio) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM personalexternos WHERE codigo IN
            (SELECT codigo FROM quest_estancias  
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 4) = ' || quote_literal(edificio) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_densidadexternos(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado EXTERNO en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un edificio. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_edificio_densidadexternos(''crue'', ''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_densidadpas(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_edificio_superficieestanciasocupadaspas(ARRAY[4,5,8,9,16], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpas)
										       AND actividad = ANY (ARRAY[4,5,8,9,16])
										       AND substring(codigo from 1 for 4) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_edificio_densidadpas(character varying) IS 'Obtiene los m2 de espacio de trabajo por PAS en un edificio de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_edificio_densidadpas(''0037'');
- select quest_edificio_densidadpas(''0037'');';

CREATE FUNCTION quest_edificio_densidadpas(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_edificio_superficieestanciasocupadaspas(ARRAY[4,5,8,9,16], $1, $2) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM personalpas)
						     AND actividad = ANY (ARRAY[4,5,8,9,16])
						     AND coddpto = upper($1)
						     AND substring(codigo FROM 1 FOR 4) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_edificio_densidadpas(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por PAS en un departamento SIGUANET y en un edificio de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_edificio_densidadpas(''B101'', ''0037'');
- select quest_edificio_densidadpas(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_densidadpas(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_edificio_superficieestanciasocupadaspas($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM personalpas WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpas)
                                           AND actividad = $1
                                           AND substring(codigo from 1 for 4) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_densidadpas(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado PAS en las estancias de una determinada actividad SIGUANET para un edificio. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_edificio_densidadpas(4, ''0018'');
- select quest_edificio_densidadpas(8, ''0018'');';

CREATE FUNCTION quest_edificio_densidadpas(tipo character varying, denominacion character varying, edificio character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_edificio_superficieestanciasocupadaspas(actlist, edificio) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM personalpas WHERE codigo IN
            (SELECT codigo FROM quest_estancias  
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 4) = ' || quote_literal(edificio) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_densidadpas(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado tipo PAS en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un edificio. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_edificio_densidadpas(''crue'', ''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_densidadpdi(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_edificio_superficieestanciasocupadaspdi(ARRAY[4,5,7], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpdi)
										       AND actividad = ANY (ARRAY[4,5,7])
										       AND substring(codigo from 1 for 4) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_edificio_densidadpdi(character varying) IS 'Obtiene los m2 de espacio de trabajo por PDI en un edificio de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_edificio_densidadpdi(''0037'');
- select quest_edificio_densidadpdi(''0037'');';

CREATE FUNCTION quest_edificio_densidadpdi(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_edificio_superficieestanciasocupadaspdi(ARRAY[4,5,7], $1, $2) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM personalpdi)
						     AND actividad = ANY (ARRAY[4,5,7])
						     AND coddpto = upper($1)
						     AND substring(codigo FROM 1 FOR 4) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_edificio_densidadpdi(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por PDI en un departamento SIGUANET y en un edificio de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_edificio_densidadpdi(''B101'', ''0037'');
- select quest_edificio_densidadpdi(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_densidadpdi(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_edificio_superficieestanciasocupadaspdi($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM personalpdi WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpdi)
                                           AND actividad = $1
                                           AND substring(codigo from 1 for 4) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_densidadpdi(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado pdi en las estancias de una determinada actividad SIGUANET para un edificio. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_edificio_densidadpdi(4, ''0018'');
- select quest_edificio_densidadpdi(8, ''0018'');';

CREATE FUNCTION quest_edificio_densidadpdi(tipo character varying, denominacion character varying, edificio character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_edificio_superficieestanciasocupadaspdi(actlist, edificio) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM personalpdi WHERE codigo IN
            (SELECT codigo FROM quest_estancias  
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 4) = ' || quote_literal(edificio) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_densidadpdi(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado tipo PDI en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un edificio. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_edificio_densidadpdi(''crue'', ''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_numadmonnoocupados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND substring(codigo from 1 for 4) = upper($1);
$_$;

CREATE FUNCTION quest_edificio_numadmonnoocupados(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    edificio varchar;
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   edificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 4) = edificio;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_numadmonnoocupados(character varying, character varying) IS 'Obtiene el nº de administraciones no ocupadas de un departamento SIGUANET en un edificio de la Universidad. 
SINTAXIS:
- select * from quest_edificio_numadmonnoocupados(''B101'', ''0037'');
- select quest_edificio_numadmonnoocupados(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_numadmonocupados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND substring(codigo from 1 for 4) = upper($1);
$_$;

CREATE FUNCTION quest_edificio_numadmonocupados(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    edificio varchar;
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   edificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 4) = edificio;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_numadmonocupados(character varying, character varying) IS 'Obtiene el nº de administraciones ocupadas de un departamento SIGUANET en un edificio de la Universidad. 
SINTAXIS:
- select * from quest_edificio_numadmonocupados(''B101'', ''0037'');
- select quest_edificio_numadmonocupados(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_numbecarios(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM becarios WHERE substring(codigo from 1 for 4) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_edificio_numbecarios(character varying) IS 'Obtiene el nº de becarios en un edificio de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_edificio_numbecarios(''0037'');
- select quest_edificio_numbecarios(''0037'');
';

CREATE FUNCTION quest_edificio_numbecarios(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM becarios
	WHERE cod_depto_centro_subunidad = upper($1)
	AND substring(codigo FROM 1 FOR 4) = upper($2)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_edificio_numbecarios(character varying, character varying) IS 'Obtiene el total de becarios de un departamento SIGUANET en un edificio de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_edificio_numbecarios(''B101'', ''0037'');
- select quest_edificio_numbecarios(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_numbecarios(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM becarios WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 4) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_edificio_numbecarios(tipo character varying, denominacion character varying, edificio character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM becarios WHERE codigo IN
            (SELECT codigo FROM quest_estancias  
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 4) = ' || quote_literal(edificio) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_numbecarios(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene el nº de BECARIOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un edificio.
SINTAXIS:
- SELECT quest_edificio_numbecarios(''crue'', ''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_numdespachosnoocupados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND substring(codigo from 1 for 4) = upper($1);
$_$;

CREATE FUNCTION quest_edificio_numdespachosnoocupados(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    edificio varchar;
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   edificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 4) = edificio;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_numdespachosnoocupados(character varying, character varying) IS 'Obtiene el nº de despachos no ocupados de un departamento SIGUANET en un edificio de la Universidad. 
SINTAXIS:
- select * from quest_edificio_numdespachosnoocupados(''B101'', ''0037'');
- select quest_edificio_numdespachosnoocupados(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_numdespachosocupados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND substring(codigo from 1 for 4) = upper($1);
$_$;

CREATE FUNCTION quest_edificio_numdespachosocupados(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    edificio varchar;
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   edificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 4) = edificio;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_numdespachosocupados(character varying, character varying) IS 'Obtiene el nº de despachos ocupados de un departamento SIGUANET en un edificio de la Universidad. 
SINTAXIS:
- select * from quest_edificio_numdespachosocupados(''B101'', ''0037'');
- select quest_edificio_numdespachosocupados(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_numestancias(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(codigo) FROM 
	(SELECT codigo from todasestancias where substring(codigo from 1 for 4) = upper($1) group by codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_edificio_numestancias(character varying) IS 'Obtiene el nº de estancias en un edificio de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_edificio_numestancias(''0037'') ;
- select quest_edificio_numestancias(''0037'');
';

CREATE FUNCTION quest_edificio_numestancias(integer, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    cuenta int8;

BEGIN
   IF $1 NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', $1;
   ELSE
	SELECT count(codigo) INTO cuenta FROM 
       (SELECT codigo from todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 4) = upper($2) GROUP BY codigo) AS foo;
      RETURN cuenta;
   END IF;
   
END
$_$;

COMMENT ON FUNCTION quest_edificio_numestancias(integer, character varying) IS 'Obtiene el nº de estancias de una determinada actividad SIGUANET en un edificio de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_edificio_numestancias(7, ''0037'');
- select quest_edificio_numestancias(8, ''0037'');';

CREATE FUNCTION quest_edificio_numestancias(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$DECLARE
    cuenta int8;
    adscripcion varchar;
    edificio varchar;
BEGIN
   adscripcion := upper($1);
   edificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

	SELECT count(codigo) INTO cuenta FROM 
	(SELECT t.codigo FROM todasestancias t, departamentossigua d 
	WHERE t.coddpto = d.cod_dpto_sigua
	AND t.coddpto = adscripcion 
	AND substring(t.codigo from 1 for 4) = edificio
	GROUP BY t.codigo) AS foo;

RETURN cuenta;

END;
$_$;

COMMENT ON FUNCTION quest_edificio_numestancias(character varying, character varying) IS 'Obtiene el nº de estancias de un departamento SIGUANET en un edificio de la Universidad
SINTAXIS:
- SELECT quest_edificio_numestancias(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_numestancias(integer, character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
   uso int4;
   adscripcion varchar;
   edificio varchar;
   cuenta int8;
	
BEGIN
uso := $1;
adscripcion := upper($2);
edificio := upper($3);
-- Control de errores
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;


   SELECT count(DISTINCT codigo) INTO cuenta FROM 
   todasestancias
   WHERE actividad = uso
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 4) =  edificio;

   RETURN cuenta;
END;
$_$;

CREATE FUNCTION quest_edificio_numestancias(tipo character varying, denominacion character varying, edificio character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
 cuenta bigint;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT count(codigo)  FROM 
           (SELECT t.codigo FROM todasestancias t, actividades a 
             WHERE substring(t.codigo from 1 for 4) = ' || quote_literal(edificio) ||
             ' AND t.actividad = a.codactividad
               AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ' GROUP BY t.codigo) AS foo;' 
  INTO cuenta;
 RETURN cuenta;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_numestancias(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene el nº de estancias de un tipo de grupo de actividad (crue, u21,activresum) y una denominación de grupo de actividad en un edificio.
SINTAXIS
SELECT quest_edificio_numestancias(''crue'',''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_numestancias(character varying, character varying, character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    agrupa varchar;
    denoactividad varchar;
    adscripcion varchar;
    edificio varchar;
    cuenta int8;
	
BEGIN
agrupa := lower($1);
adscripcion := upper($3);
edificio := upper($4);

IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
   RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
END IF;

IF agrupa = 'crue' THEN
	denoactividad := upper($2);
        IF denoactividad NOT IN ( SELECT crue FROM actividades) THEN
	   RAISE EXCEPTION 'La actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
		SELECT count(codigo) INTO cuenta FROM 
		(SELECT t.codigo FROM todasestancias t, departamentossigua d, actividades a 
		WHERE t.coddpto = d.cod_dpto_sigua
		AND t.actividad = a.codactividad
		AND t.coddpto = adscripcion 
		AND a.crue = denoactividad
		AND substring(t.codigo from 1 for 4) = edificio
		GROUP BY t.codigo) AS foo;
		RETURN cuenta;
ELSEIF agrupa = 'u21' THEN
	denoactividad := upper($2);
        IF denoactividad NOT IN ( SELECT u21 FROM actividades) THEN
	   RAISE EXCEPTION 'La actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
		SELECT count(codigo) INTO cuenta FROM 
		(SELECT t.codigo FROM todasestancias t, departamentossigua d, actividades a 
		WHERE t.coddpto = d.cod_dpto_sigua
		AND t.actividad = a.codactividad
		AND t.coddpto = adscripcion 
		AND a.u21 = denoactividad
		AND substring(t.codigo from 1 for 4) = edificio
		GROUP BY t.codigo) AS foo;
		RETURN cuenta;
ELSEIF agrupa = 'activresum' THEN
	denoactividad := $2;
        IF denoactividad NOT IN ( SELECT activresum FROM actividades) THEN
	   RAISE EXCEPTION 'La actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
		SELECT count(codigo) INTO cuenta FROM 
		(SELECT t.codigo FROM todasestancias t, departamentossigua d, actividades a 
		WHERE t.coddpto = d.cod_dpto_sigua
		AND t.actividad = a.codactividad
		AND t.coddpto = adscripcion 
		AND a.activresum = denoactividad
		AND substring(t.codigo from 1 for 4) = edificio
		GROUP BY t.codigo) AS foo;
		RETURN cuenta;
ELSE
     RAISE exception 'No existe un campo valido con ese nombre: (%). Debe de ser u21, activresum o crue ', $1;
END IF;

END;

$_$;

CREATE FUNCTION quest_edificio_numestanciasdocentes(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(codigo) FROM 
	(SELECT t.codigo from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE upper(a.activresum) = 'DOCENCIA'
         AND substring(codigo from 1 for 4) = $1
         group by t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_edificio_numestanciasdocentes(character varying) IS 'Obtiene el nº de estancias de un edificio de la Universidad cuya actividad pertenece al grupo "Docencia" de la tabla actividades. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_edificio_numestanciasdocentes(''0011'');
- select quest_edificio_numestanciasdocentes(''0011'');';

CREATE FUNCTION quest_edificio_numestanciasdocentes(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(codigo) FROM 
	(SELECT t.codigo from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE upper(a.activresum) = 'DOCENCIA'
	 AND t.coddpto = upper($1)
         AND substring(t.codigo from 1 for 4) = upper($2)
         group by t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_edificio_numestanciasdocentes(character varying, character varying) IS 'Obtiene el nº de estancias adscritas a un departamento SIGUANET en un edificio de la Universidad cuya actividad pertenece al grupo "Docencia" de la tabla actividades. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_edificio_numestanciasdocentes(''B101'', ''0037'');
- select quest_edificio_numestanciasdocentes(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_numestanciasnoocupadas(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1
	AND substring(codigo from 1 for 4) = upper($2);
$_$;

CREATE FUNCTION quest_edificio_numestanciasnoocupadas(tipo character varying, denominacion character varying, edificio character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
 cuenta bigint;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT count(DISTINCT t.codigo) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo NOT IN (SELECT codigo FROM todaspersonas)
             AND substring(t.codigo from 1 for 4) = ' || quote_literal(edificio) || ';' INTO cuenta;
 RETURN cuenta;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_numestanciasnoocupadas(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene el nº de estancias no ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un edificio.
SINTAXIS
SELECT quest_edificio_numestanciasnoocupadas(''crue'',''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_numestanciasocupadas(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND substring(codigo from 1 for 4) = upper($1)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_edificio_numestanciasocupadas(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND coddpto = upper($1)
	AND substring(codigo from 1 for 4) = upper($2)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_edificio_numestanciasocupadas(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1
	AND substring(codigo from 1 for 4) = upper($2);
$_$;

CREATE FUNCTION quest_edificio_numestanciasocupadas(tipo character varying, denominacion character varying, edificio character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
 cuenta bigint;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT count(DISTINCT t.codigo) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo IN (SELECT codigo FROM todaspersonas)
             AND substring(t.codigo from 1 for 4) = ' || quote_literal(edificio) || ';' INTO cuenta;
 RETURN cuenta;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_numestanciasocupadas(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene el nº de estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un edificio.
SINTAXIS
SELECT quest_edificio_numestanciasocupadas(''crue'',''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_numestanciasutiles(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(*) FROM 
	(SELECT count(t.codigo) from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE a.util = true AND substring(codigo from 1 for 4) = upper($1) GROUP BY t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_edificio_numestanciasutiles(character varying) IS 'Obtiene el nº de estancias útiles en un edificio de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_edificio_numestanciasutiles(''0037'');
- select quest_edificio_numestanciasutiles(''0037'');
';

CREATE FUNCTION quest_edificio_numestanciasutiles(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(*) FROM 
	(SELECT count(t.codigo)
	 FROM todasestancias t 
	 JOIN actividades a ON t.actividad = a.codactividad 
         WHERE a.util = true 
	 AND t.coddpto = upper($1)
	 AND substring(t.codigo from 1 for 4) = upper($2) GROUP BY t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_edificio_numestanciasutiles(character varying, character varying) IS 'Obtiene el nº de estancias útiles adscritas a un departamento SIGUANET en un edificio de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_edificio_numestanciasutiles(''B101'', ''0037'');
- select quest_edificio_numestanciasutiles(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_numexternos(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalexternos WHERE substring(codigo from 1 for 4) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_edificio_numexternos(character varying) IS 'Obtiene el nº de externos en un edificio de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_edificio_numexternos(''0037'');
- select quest_edificio_numexternos(''0037'');
';

CREATE FUNCTION quest_edificio_numexternos(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalexternos
	WHERE cod_dpto_sigua = upper($1)
	AND substring(codigo FROM 1 FOR 4) = upper($2)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_edificio_numexternos(character varying, character varying) IS 'Obtiene el total de externos de un departamento SIGUANET en un edificio de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_edificio_numexternos(''B101'', ''0037'');
- select quest_edificio_numexternos(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_numexternos(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM personalexternos WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 4) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_edificio_numexternos(tipo character varying, denominacion character varying, edificio character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM personalexternos WHERE codigo IN
            (SELECT codigo FROM quest_estancias  
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 4) = ' || quote_literal(edificio) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_numexternos(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene el nº de empleados EXTERNOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un edificio.
SINTAXIS:
- SELECT quest_edificio_numexternos(''crue'', ''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_numpas(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalpas WHERE substring(codigo from 1 for 4) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_edificio_numpas(character varying) IS 'Obtiene el nº de PAS en un edificio de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_edificio_numpas(''0037'');
- select quest_edificio_numpas(''0037'');
';

CREATE FUNCTION quest_edificio_numpas(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalpas
	WHERE cod_unidad = upper($1)
	AND substring(codigo FROM 1 FOR 4) = upper($2)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_edificio_numpas(character varying, character varying) IS 'Obtiene el total de PAS de un departamento SIGUANET en un edificio de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_edificio_numpas(''B101'', ''0037'');
- select quest_edificio_numpas(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_numpas(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM personalpas WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 4) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_edificio_numpas(tipo character varying, denominacion character varying, edificio character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM personalpas WHERE codigo IN
            (SELECT codigo FROM quest_estancias  
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 4) = ' || quote_literal(edificio) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_numpas(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene el nº de PAS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un edificio.
SINTAXIS:
- SELECT quest_edificio_numpas(''crue'', ''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_numpdi(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalpdi WHERE substring(codigo from 1 for 4) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_edificio_numpdi(character varying) IS 'Obtiene el nº de PDI en un edificio de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_edificio_numpdi(''0037'');
- select quest_edificio_numpdi(''0037'');
';

CREATE FUNCTION quest_edificio_numpdi(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalpdi
	WHERE cod_depto = upper($1)
	AND substring(codigo FROM 1 FOR 4) = upper($2)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_edificio_numpdi(character varying, character varying) IS 'Obtiene el total de PDI de un departamento SIGUANET en un edificio de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_edificio_numpdi(''B101'', ''0037'');
- select quest_edificio_numpdi(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_numpdi(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM personalpdi WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 4) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_edificio_numpdi(tipo character varying, denominacion character varying, edificio character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM personalpdi WHERE codigo IN
            (SELECT codigo FROM quest_estancias  
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 4) = ' || quote_literal(edificio) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_numpdi(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene el nº de PDI ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un edificio.
SINTAXIS:
- SELECT quest_edificio_numpdi(''crue'', ''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_numpdicargos(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalpdi_cargos WHERE substring(codigo from 1 for 4) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_edificio_numpdicargos(character varying) IS 'Obtiene el nº de PDI que desempeñan cargo en un edificio de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_edificio_numpdicargos(''0037'');
- select quest_edificio_numpdicargos(''0037'');
';

CREATE FUNCTION quest_edificio_numpdicargos(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalpdi_cargos p
	JOIN cargos_dpto cd ON p.cod_cargo = cd.cod_cargo
	WHERE cd.coddpto = upper($1)
	AND substring(p.codigo from 1 for 4) = upper($2) 
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_edificio_numpdicargos(character varying, character varying) IS 'Obtiene el nº de PDI que desempeñan cargo en un departamento sigua y en un edificio de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_edificio_numpdicargos(''B101'', ''0037'');
- select quest_edificio_numpdicargos(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_numpersonas(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
    SELECT count(DISTINCT nif) FROM todaspersonas
    WHERE substring (codigo from 1 for 4) = $1 AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_edificio_numpersonas(character varying) IS 'Obtiene el nº de personas en un edificio de la Universidad.
Se ejecuta de dos formas:
- select * from quest_edificio_numpersonas(''0037'');
- select quest_edificio_numpersonas(''0037'');
';

CREATE FUNCTION quest_edificio_numpersonas(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$DECLARE
    cuenta int8;
    adscripcion varchar;
    edificio varchar;
BEGIN
   adscripcion := upper($1);
   edificio := $2;
   IF adscripcion NOT IN (SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT nif) INTO cuenta 
   FROM todaspersonas
   WHERE cod_depto = upper(adscripcion)
   AND substring(codigo FROM 1 FOR 4) = edificio
   AND codigo != '0000PB997';

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_numpersonas(character varying, character varying) IS 'Obtiene el nº de personas de un departamento sigua en un edificio de la Universidad, indicando si es pas, pdi, becario, externo o alguna de sus combinaciones
SINTAXIS:
- SELECT quest_edificio_numpersonas(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_numpersonas(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM todaspersonas WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 4) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_edificio_numpersonas(tipo character varying, denominacion character varying, edificio character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM todaspersonas WHERE codigo IN
            (SELECT codigo FROM quest_estancias  WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
            ' AND substring(codigo FROM 1 FOR 4) = ' || quote_literal(edificio) || ');' INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_numpersonas(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene el nº de personas ubicadas en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad, en un edificio.
SINTAXIS:
- SELECT quest_edificio_numpersonas(''crue'', ''DOCENCIA'', ''0037'');';

CREATE VIEW quest_departamentos AS
    SELECT unidades.cod_unidad AS cod, unidades.txt_unidad AS txt, false AS es_centro, false AS es_dpto, true AS es_unidad FROM public.unidades UNION SELECT departamentos.cod_dpto AS cod, departamentos.txt_dpto AS txt, false AS es_centro, true AS es_dpto, false AS es_unidad FROM public.departamentos ORDER BY 2;

COMMENT ON VIEW quest_departamentos IS 'Vista para crear objetos de tipo departamento SIGUANET';

CREATE VIEW quest_estancias  AS
    SELECT 0 AS gid, te.codigo, public.st_astext(te.geometria) AS wkt, public.st_srid(te.geometria) AS srid, "substring"((te.codigo)::text, 1, 6) AS codplantaedif, "substring"((te.codigo)::text, 5, 2) AS enumplanta, (((e.txt_edificio)::text || ' '::text) || "substring"((te.codigo)::text, 5, 2)) AS denoplanta, ((e.cod_zona)::text || (e.cod_edificio)::text) AS codedificio, e.txt_edificio AS denoedificio, z.cod_zona AS codzona, z.txt_zona AS denozona, te.actividad, a.txt_actividad AS denoactividad, a.activresum AS denogrupo, a.crue AS denocrue, a.u21 AS denou21, te.coddpto, ds.txt AS denodpto, ds.es_centro, ds.es_dpto, ds.es_unidad, te.denominaci AS denoestancia, te.observacio AS observaestancia FROM public.todasestancias te, public.edificios e, public.zonas z, public.actividades a, quest_departamentos ds WHERE (((("substring"((te.codigo)::text, 1, 4) = ((e.cod_zona)::text || (e.cod_edificio)::text)) AND ("substring"((te.codigo)::text, 1, 2) = (z.cod_zona)::text)) AND (te.actividad = a.codactividad)) AND ((te.coddpto)::text = (ds.cod)::text)) ORDER BY te.codigo;

COMMENT ON VIEW quest_estancias  IS 'Entidad para representar objetos de tipo estancia';

CREATE FUNCTION quest_edificio_obteneradmonnoocupados(character varying) RETURNS SETOF quest_estancias 
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias 
WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
AND actividad = 8
AND codedificio = $1;
$_$;

CREATE FUNCTION quest_edificio_obteneradmonnoocupados(character varying, character varying) RETURNS SETOF quest_estancias 
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND coddpto = upper($1)
	AND codedificio = upper($2);
$_$;

CREATE FUNCTION quest_edificio_obteneradmonocupados(character varying) RETURNS SETOF quest_estancias 
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias 
WHERE codigo IN (SELECT codigo FROM todaspersonas)
AND actividad = 8
AND codedificio = $1;
$_$;

CREATE FUNCTION quest_edificio_obteneradmonocupados(character varying, character varying) RETURNS SETOF quest_estancias 
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND coddpto = upper($1)
	AND codedificio = upper($2);
$_$;

CREATE FUNCTION quest_edificio_obtenerdespachosnoocupados(character varying) RETURNS SETOF quest_estancias 
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias 
WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
AND actividad = 7
AND codedificio = $1;
$_$;

CREATE FUNCTION quest_edificio_obtenerdespachosnoocupados(character varying, character varying) RETURNS SETOF quest_estancias 
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND coddpto = upper($1)
	AND codedificio = upper($2);
$_$;

CREATE FUNCTION quest_edificio_obtenerdespachosocupados(character varying) RETURNS SETOF quest_estancias 
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias 
WHERE codigo IN (SELECT codigo FROM todaspersonas)
AND actividad = 7
AND codedificio = $1;
$_$;

CREATE FUNCTION quest_edificio_obtenerdespachosocupados(character varying, character varying) RETURNS SETOF quest_estancias 
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND coddpto = upper($1)
	AND codedificio = upper($2);
$_$;

CREATE FUNCTION quest_edificio_obtenerestancias(character varying, character varying) RETURNS SETOF quest_estancias 
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila quest_estancias%ROWTYPE;
    adscripcion varchar;
    edificio varchar;
BEGIN
   adscripcion := upper($1);
   edificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   FOR fila IN 
	SELECT * FROM quest_estancias  
	WHERE coddpto = adscripcion
	AND codedificio = upper(edificio)
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_obtenerestancias(character varying, character varying) IS 'Obtiene todas las estancias de un departamento sigua en un edificio de la Universidad
Ejemplo:
SELECT * FROM quest_edificio_obtenerestancias(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_obtenerestancias(integer, character varying) RETURNS SETOF quest_estancias 
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila quest_estancias%ROWTYPE;
    act integer;
    edificio varchar;
BEGIN
   act := $1;
   edificio := upper($2);
   IF act NOT IN (SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION '% no es una actividad en la tabla actividades.', act;
   END IF;

   FOR fila IN 
	SELECT * FROM quest_estancias  
	WHERE actividad = act
	AND codedificio = upper(edificio)
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_obtenerestancias(integer, character varying) IS 'Obtiene todas las estancias deesignadas para un uso SIGUANET en un edificio de la Universidad
Ejemplo:
SELECT * FROM quest_edificio_obtenerestancias(7, ''0037'');';

CREATE FUNCTION quest_edificio_obtenerestancias(tipo character varying, denominacion character varying, edificio character varying) RETURNS SETOF quest_estancias 
    LANGUAGE plpgsql
    AS $$
DECLARE
  validacion integer;
  c refcursor;
  estancia quest_estancias%ROWTYPE;
  tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
        WHEN 'activresum' THEN 'denogrupo'
        WHEN 'crue' THEN 'denocrue'
        WHEN 'u21' THEN 'denou21'
        END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_estancias  WHERE substring(codigo from 1 for 4) = ' || quote_literal(edificio) || ' AND lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || ';';
  LOOP
   FETCH c INTO estancia;
   EXIT WHEN NOT FOUND;
   RETURN NEXT estancia;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_obtenerestancias(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene las estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un edificio.
SINTAXIS
SELECT quest_ua_obtenerestancias(''crue'',''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_obtenerestanciasdocentes(character varying) RETURNS SETOF quest_estancias 
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias  
	WHERE upper(denogrupo) = 'DOCENCIA'
	AND codedificio = $1;
$_$;

CREATE FUNCTION quest_edificio_obtenerestanciasdocentes(character varying, character varying) RETURNS SETOF quest_estancias 
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias  
	WHERE upper(denogrupo) = 'DOCENCIA'
	AND coddpto = upper($1)
	AND codedificio = upper($2);
$_$;

CREATE FUNCTION quest_edificio_obtenerestanciasnoocupadas(integer, character varying) RETURNS SETOF quest_estancias 
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias  
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1
	AND codedificio = upper($2);

$_$;

CREATE FUNCTION quest_edificio_obtenerestanciasnoocupadas(tipo character varying, denominacion character varying, edificio character varying) RETURNS SETOF quest_estancias 
    LANGUAGE plpgsql
    AS $$
DECLARE
 validacion integer;
 c refcursor;
 estancia quest_estancias%ROWTYPE;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 SELECT INTO tipo_ CASE lower(tipo)
  WHEN 'activresum' THEN 'denogrupo'
  WHEN 'crue' THEN 'denocrue'
  WHEN 'u21' THEN 'denou21'
 END;
 OPEN c FOR EXECUTE 'SELECT * FROM quest_estancias  
                      WHERE lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || 
                    ' AND codigo NOT IN (SELECT codigo FROM todaspersonas) 
                      AND substring(codigo from 1 for 4) = ' || quote_literal(edificio) || ';' ;
 LOOP
  FETCH c INTO estancia;
  EXIT WHEN NOT FOUND;
  RETURN NEXT estancia;
 END LOOP;
 RETURN;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_obtenerestanciasnoocupadas(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene las estancias no ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un en un edificio.
SINTAXIS
SELECT * FROM quest_edificio_obtenerestanciasnoocupadas(''crue'',''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_obtenerestanciasocupadas(character varying) RETURNS SETOF quest_estancias 
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias  
	WHERE (actividad <= 50 OR actividad = 99)
	AND codigo IN (SELECT codigo FROM todaspersonas)
	AND codedificio = $1
	AND codigo != '0000PB997';

$_$;

CREATE FUNCTION quest_edificio_obtenerestanciasocupadas(character varying, character varying) RETURNS SETOF quest_estancias 
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias  
	WHERE (actividad <= 50 OR actividad = 99)
	AND codigo IN (SELECT codigo FROM todaspersonas)
	AND coddpto = upper($1)
	AND codedificio = upper($2)
	AND codigo != '0000PB997';

$_$;

CREATE FUNCTION quest_edificio_obtenerestanciasocupadas(integer, character varying) RETURNS SETOF quest_estancias 
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias  
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1
	AND codedificio = upper($2);

$_$;

CREATE FUNCTION quest_edificio_obtenerestanciasocupadas(tipo character varying, denominacion character varying, edificio character varying) RETURNS SETOF quest_estancias 
    LANGUAGE plpgsql
    AS $$
DECLARE
 validacion integer;
 c refcursor;
 estancia quest_estancias%ROWTYPE;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 SELECT INTO tipo_ CASE lower(tipo)
  WHEN 'activresum' THEN 'denogrupo'
  WHEN 'crue' THEN 'denocrue'
  WHEN 'u21' THEN 'denou21'
 END;
 OPEN c FOR EXECUTE 'SELECT * FROM quest_estancias 
                      WHERE lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || 
                    ' AND codigo IN (SELECT codigo FROM todaspersonas) 
                      AND substring(codigo from 1 for 4) = ' || quote_literal(edificio) || ';' ;
 LOOP
  FETCH c INTO estancia;
  EXIT WHEN NOT FOUND;
  RETURN NEXT estancia;
 END LOOP;
 RETURN;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_obtenerestanciasocupadas(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene las estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un en un edificio.
SINTAXIS
SELECT * FROM quest_edificio_obtenerestanciasocupadas(''crue'',''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_obtenerestanciasutiles(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila quest_estancias%ROWTYPE;
    adscripcion varchar;
    edificio varchar;	
BEGIN
   adscripcion := upper($1);
   edificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   FOR fila IN 
	SELECT * FROM quest_estancias t JOIN actividades a ON t.actividad = a.codactividad 
	WHERE a.util = true 
	AND t.coddpto = adscripcion
	AND codedificio = upper(edificio)
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_obtenerestanciasutiles(character varying, character varying) IS 'Obtiene todas las estancias útiles de un departamento sigua en un edificio de la Universidad
Ejemplo:
SELECT * FROM quest_edificio_obtenerestanciasutiles(''B101'', ''0037'');';

CREATE TYPE quest_edificio AS
   (codigo text,
    denominacion_edificio text,
    checklist_plantas integer[],
    visibilidad boolean,
    rotacion double precision,
    traslacion double precision,
    zona text,
    denominacion_zona text);

CREATE TYPE quest_plantabase AS
   (planta text,
    indice integer);

CREATE FUNCTION quest_nombreplanta(p quest_plantabase)
  RETURNS text AS
$BODY$
DECLARE
BEGIN
 IF p.planta = 'pb' THEN
  RETURN 'PLANTA BAJA';
 ELSIF p.planta = 'ps' THEN
  RETURN 'SÓTANO';
 ELSIF p.planta LIKE 'p%' THEN
  RETURN 'PLANTA ' || p.indice::text;
 ELSIF p.planta LIKE 's%' THEN
  RETURN 'SÓTANO ' || abs(p.indice)::text;
 ELSE
  RETURN '-';
 END IF;
END;
$BODY$
LANGUAGE plpgsql IMMUTABLE;

CREATE VIEW quest_esquemaplantas AS 
 SELECT columns.table_name, columns.column_name, substr(columns.column_name::text, 4) AS planta, 
        CASE
            WHEN columns.column_name::text = 'sigpb'::text THEN 0
            WHEN columns.column_name::text = 'sigps'::text THEN -1
            WHEN columns.column_name::text ~~ 'sigp%'::text THEN substr(columns.column_name::text, 5)::integer
            WHEN columns.column_name::text ~~ 'sigs%'::text THEN substr(columns.column_name::text, 5)::integer * (-1)
            ELSE (-1000)
        END AS indice
   FROM information_schema.columns
  WHERE columns.table_schema::text = 'public'::text AND columns.table_name::text = 'edificios'::text AND (columns.column_name::text = 'sigpb'::text OR columns.column_name::text ~ '^sig[p|s][0-9]{1,3}$'::text) AND columns.data_type::text = 'boolean'::text;

CREATE FUNCTION quest_plantastoarrayexp()
  RETURNS text AS
$BODY$
DECLARE
r quest_esquemaplantas%ROWTYPE;
arrayexp text := '';
BEGIN
 FOR r IN SELECT * FROM quest_esquemaplantas LOOP
  IF arrayexp = '' THEN
   arrayexp := ' ARRAY[';
  ELSE
   arrayexp := arrayexp || ',';
  END IF;
  arrayexp := arrayexp || r.indice || ',' || r.table_name || '.' || quote_ident(r.column_name) || '::int';
 END LOOP;
 arrayexp := arrayexp || '] ';
 RETURN arrayexp;
END;
$BODY$
  LANGUAGE plpgsql STABLE;

CREATE FUNCTION quest_edificios()
  RETURNS SETOF quest_edificio AS
$BODY$
DECLARE

BEGIN
  RETURN QUERY EXECUTE 'SELECT  edificios.cod_zona::text || edificios.cod_edificio::text,'
                       || 'edificios.txt_edificio::text,'
                       || quest_plantastoarrayexp() || ','
                       || 'edificios.visibilidad,'
                       || 'edificios.rotacion::double precision,'
                       || 'edificios.traslacion::double precision,'
                       || 'zonas.cod_zona::text,'
                       || 'zonas.txt_zona::text'
                       || ' FROM edificios JOIN zonas ON edificios.cod_zona::text = zonas.cod_zona::text;';
END;
$BODY$
LANGUAGE plpgsql VOLATILE;


CREATE FUNCTION quest_plantasbase()
  RETURNS SETOF quest_plantabase AS
$BODY$
DECLARE
r quest_esquemaplantas%ROWTYPE;
b boolean := false;
p quest_plantabase;
BEGIN
 FOR r IN SELECT * FROM quest_esquemaplantas LOOP
  EXECUTE 'SELECT true FROM edificios WHERE ' 
          || quote_ident(r.column_name) 
          || ' = true LIMIT 1;' 
          INTO b;
  IF b THEN
   p.planta := r.planta;
   p.indice := r.indice;
   b := false;
   RETURN NEXT p;
  END IF;
 END LOOP;
 RETURN;
END;
$BODY$
LANGUAGE plpgsql VOLATILE;

CREATE TYPE quest_plantazona AS
   (zona text,
    planta text,
    indice integer);

CREATE FUNCTION quest_plantaszona()
  RETURNS SETOF quest_plantazona AS
$BODY$
DECLARE
r quest_esquemaplantas%ROWTYPE;
BEGIN
 FOR r IN SELECT * FROM quest_esquemaplantas LOOP
  RETURN QUERY EXECUTE 'SELECT cod_zona::text AS zona, '
                       || quote_literal(r.planta) || '::text AS planta, '
                       || r.indice::text || '::integer AS indice FROM edificios'
                       || ' WHERE ' || quote_ident(r.column_name) || ' = true GROUP BY cod_zona;';
 END LOOP;
 RETURN;
END;
$BODY$
LANGUAGE plpgsql VOLATILE;

CREATE TYPE quest_plantaedificio AS
   (codigo text,
    planta text,
    denominacion_planta text,
    edificio text,
    denominacion_edificio text,
    checklist_plantas integer[],
    zona text,
    denominacion_zona text,
    indice integer);

CREATE FUNCTION quest_plantasedificio()
  RETURNS SETOF quest_plantaedificio AS
$BODY$
DECLARE
r quest_esquemaplantas%ROWTYPE;
arrayexp text;
BEGIN
 FOR r IN SELECT * FROM quest_esquemaplantas LOOP
  RETURN QUERY EXECUTE 'SELECT edificios.cod_zona::text || edificios.cod_edificio::text || ' || quote_literal(r.planta) || '::text,' 
                       || quote_literal(r.planta) || '::text,'
                       || 'edificios.txt_edificio::text || ' || quote_literal('(' || quest_nombreplanta(ROW(r.planta, r.indice)) || ')') || '::text,'
                       || 'edificios.cod_zona::text || edificios.cod_edificio::text,'
                       || 'edificios.txt_edificio::text,'
                       || quest_plantastoarrayexp() || ','
                       || 'zonas.cod_zona::text,'
                       || 'zonas.txt_zona::text,'
                       || r.indice::text || '::integer'
                       || ' FROM edificios JOIN zonas ON edificios.cod_zona::text = zonas.cod_zona::text'
                       || ' WHERE ' || quote_ident(r.column_name) || ' = true;';
 END LOOP;
 RETURN;
END;
$BODY$
LANGUAGE plpgsql VOLATILE;

CREATE FUNCTION quest_edificio_obtenerplantas(text) RETURNS SETOF quest_plantaedificio
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_plantasedificio() WHERE edificio = $1 ORDER BY indice;
$_$;

CREATE FUNCTION quest_edificio_obtenerplantas(character varying, character varying) RETURNS SETOF quest_plantaedificio
    LANGUAGE sql
    AS $_$
	SELECT DISTINCT p.* 
	FROM quest_plantasedificio() p JOIN todasestancias e ON p.codigo = substring(e.codigo FROM 1 FOR 6)
	WHERE edificio = $1 AND e.coddpto = $2
	ORDER BY indice;

$_$;

CREATE FUNCTION quest_edificio_obtenerplantas(character varying, integer) RETURNS SETOF quest_plantaedificio
    LANGUAGE sql
    AS $_$
	SELECT DISTINCT p.* 
	FROM quest_plantasedificio() p JOIN todasestancias e ON p.codigo = substring(e.codigo FROM 1 FOR 6)
	WHERE p.edificio = $1 AND e.actividad = $2
	ORDER BY p.indice;

$_$;

CREATE FUNCTION quest_edificio_obtenerplantas(tipo character varying, denominacion character varying, edificio character varying) RETURNS SETOF quest_plantaedificio
    LANGUAGE plpgsql
    AS $$
DECLARE
  validacion integer;
  c refcursor;
  planta quest_plantaedificio;
  tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
        WHEN 'activresum' THEN 'denogrupo'
        WHEN 'crue' THEN 'denocrue'
        WHEN 'u21' THEN 'denou21'
        END;
  OPEN c FOR EXECUTE 'SELECT DISTINCT p.* 
	               FROM quest_plantasedificio() p JOIN quest_estancias e ON p.codigo = substring(e.codigo FROM 1 FOR 6)
	               WHERE p.edificio = ' || quote_literal(upper(edificio)) || 'AND lower(e.' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) ||
	             ' ORDER BY p.indice;';

  LOOP
   FETCH c INTO planta;
   EXIT WHEN NOT FOUND;
   RETURN NEXT planta;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_obtenerplantas(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene las plantas de un determinado edificio en las que existen estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS
SELECT * FROM quest_edificio_obtenerplantas(''crue'',''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_superficie(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
SELECT sum(st_area(geometria)) FROM todasestancias
WHERE substring(codigo from 1 for 4) = upper($1);
$_$;

COMMENT ON FUNCTION quest_edificio_superficie(character varying) IS 'Obtiene la superficie total de un edificio de la Universidad.';

CREATE FUNCTION quest_edificio_superficie(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria)) 
	FROM todasestancias
	WHERE coddpto = upper($1)
	AND substring(codigo from 1 for 4) = upper($2);
$_$;

COMMENT ON FUNCTION quest_edificio_superficie(character varying, character varying) IS 'Obtiene la superficie total de las estancias de un departamento SIGUANET en un edificio de la Universidad.';

CREATE FUNCTION quest_edificio_superficie(integer, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria)) 
	FROM todasestancias
	WHERE actividad = $1
	AND substring(codigo from 1 for 4) = upper($2);
$_$;

COMMENT ON FUNCTION quest_edificio_superficie(integer, character varying) IS 'Obtiene la superficie total de las estancias designadas para un uso SIGUANET en un edificio de la Universidad.';

CREATE FUNCTION quest_edificio_superficie(tipo character varying, denominacion character varying, edificio character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
 superficie double precision;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT sum(st_area(geometria)) FROM 
           (SELECT t.geometria FROM todasestancias t, actividades a 
             WHERE substring(t.codigo from 1 for 4) = ' || quote_literal(edificio) ||
             ' AND t.actividad = a.codactividad
               AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ') AS foo;'
  INTO superficie;
 RETURN superficie;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_superficie(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene la superficie que ocupan las estancias de un tipo de grupo de actividad (crue, u21,activresum) y una denominación de grupo de actividad en un edificio.
SINTAXIS
SELECT quest_edificio_superficie(''crue'',''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_superficieadmonnoocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    edificio varchar;
BEGIN
   edificio := upper($1);
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND substring(codigo from 1 for 4) = edificio;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_edificio_superficieadmonnoocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    edificio varchar;
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   edificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 4) = edificio;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_superficieadmonnoocupados(character varying, character varying) IS 'Obtiene la superficie de administraciones no ocupadas de un departamento SIGUANET en un edificio de la Universidad. 
SINTAXIS:
- select * from quest_edificio_superficieadmonnoocupados(''B101'', ''0037'');
- select quest_edificio_superficieadmonnoocupados(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_superficieadmonocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    edificio varchar;
BEGIN
   edificio := upper($1);
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND substring(codigo from 1 for 4) = edificio;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_edificio_superficieadmonocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    edificio varchar;
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   edificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 4) = edificio;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_superficieadmonocupados(character varying, character varying) IS 'Obtiene la superficie de administraciones ocupadas de un departamento SIGUANET en un edificio de la Universidad. 
SINTAXIS:
- select * from quest_edificio_superficieadmonocupados(''B101'', ''0037'');
- select quest_edificio_superficieadmonocupados(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_superficiedespachosnoocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    edificio varchar;
BEGIN
   edificio := upper($1);
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND substring(codigo from 1 for 4) = edificio;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_edificio_superficiedespachosnoocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    edificio varchar;
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   edificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 4) = edificio;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_superficiedespachosnoocupados(character varying, character varying) IS 'Obtiene la superficie de despachos no ocupados de un departamento SIGUANET en un edificio de la Universidad. 
SINTAXIS:
- select * from quest_edificio_superficiedespachosnoocupados(''B101'', ''0037'');
- select quest_edificio_superficiedespachosnoocupados(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_superficiedespachosocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    edificio varchar;
BEGIN
   edificio := upper($1);
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND substring(codigo from 1 for 4) = edificio;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_edificio_superficiedespachosocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    edificio varchar;
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   edificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 4) = edificio;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_superficiedespachosocupados(character varying, character varying) IS 'Obtiene la superficie de despachos ocupados de un departamento SIGUANET en un edificio de la Universidad. 
SINTAXIS:
- select * from quest_edificio_superficiedespachosocupados(''B101'', ''0037'');
- select quest_edificio_superficiedespachosocupados(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_superficiedocente(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE upper(a.activresum) = 'DOCENCIA'
        AND substring(codigo from 1 for 4) = $1;
$_$;

COMMENT ON FUNCTION quest_edificio_superficiedocente(character varying) IS 'Obtiene la superficie total de las estancias docentes de un edificio de la Universidad.
Se ejecuta de dos formas:
- select * from quest_edificio_superficiedocente(''0011'');
- select quest_edificio_superficiedocente(''0011'');';

CREATE FUNCTION quest_edificio_superficiedocente(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE upper(a.activresum) = 'DOCENCIA'
	AND t.coddpto = upper($1)
        AND substring(t.codigo from 1 for 4) = upper($2);
$_$;

COMMENT ON FUNCTION quest_edificio_superficiedocente(character varying, character varying) IS 'Obtiene la superficie total de las estancias docentes de una departamento SIGUANET en un edificio de la Universidad.
Se ejecuta de dos formas:
- select * from quest_edificio_superficiedocente(''B101'', ''0037'');
- select quest_edificio_superficiedocente(''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_superficieestanciasnoocupadas(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4;
  edificio varchar;
  superficie float8;
BEGIN
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;

        uso := $1;
        edificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = uso
        AND substring(codigo from 1 for 4) = edificio;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_superficieestanciasnoocupadas(integer, character varying) IS 'Obtiene la superficie de las estancias no ocupadas de una determinada actividad SIGUANET en un edificio de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_edificio_superficieestanciasnoocupadas(50, ''0037'');
- select quest_edificio_superficieestanciasnoocupadas(50, ''0037'');';

CREATE FUNCTION quest_edificio_superficieestanciasnoocupadas(tipo character varying, denominacion character varying, edificio character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
 superficie double precision;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT sum(st_area(t.geometria)) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo NOT IN (SELECT codigo FROM todaspersonas) 
             AND substring(t.codigo from 1 for 4) = ' || quote_literal(edificio) || ';' INTO superficie;
 RETURN superficie;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_superficieestanciasnoocupadas(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene la superficie de las estancias no ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un en un edificio.
SINTAXIS
SELECT quest_edificio_superficieestanciasnoocupadas(''crue'',''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_superficieestanciasocupadas(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND substring(codigo from 1 for 4) = upper($1)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_edificio_superficieestanciasocupadas(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4;
  edificio varchar;
  superficie float8;
BEGIN
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;

        uso := $1;
        edificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = uso
        AND substring(codigo from 1 for 4) = edificio;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_superficieestanciasocupadas(integer, character varying) IS 'Obtiene la superficie de las estancias ocupadas de una determinada actividad SIGUANET en un edificio de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_edificio_superficieestanciasocupadas(50, ''0037'');
- select quest_edificio_superficieestanciasocupadas(50, ''0037'');';

CREATE FUNCTION quest_edificio_superficieestanciasocupadas(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4[];
  edificio varchar;
  superficie float8;
BEGIN
        uso := $1;
        edificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = ANY (uso)
        AND substring(codigo from 1 for 4) = edificio;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_superficieestanciasocupadas(integer[], character varying) IS 'Obtiene la superficie de las estancias ocupadas de una lista de actividades SIGUANET en un edificio de la Universidad.
Se ejecuta de dos formas:
- select * from quest_edificio_superficieestanciasocupadas(ARRAY[1,2,3,5], ''0037'');
- select quest_edificio_superficieestanciasocupadas(ARRAY[1,2,3,5], ''0037'');';

CREATE FUNCTION quest_edificio_superficieestanciasocupadas(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND coddpto = upper($1)
	AND substring(codigo from 1 for 4) = upper($2)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_edificio_superficieestanciasocupadas(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4[];
  edificio varchar;
  adscripcion varchar;
  superficie float8;
BEGIN
        uso := $1;
	adscripcion := upper($2);
        edificio := upper($3);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = ANY (uso)
	AND coddpto = adscripcion
        AND substring(codigo from 1 for 4) = edificio
	AND codigo != '0000PB997';

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_superficieestanciasocupadas(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias ocupadas y adscritas a un departamento SIGUANET de una lista de actividades SIGUANET en un edificio de la Universidad.
Se ejecuta de dos formas:
- select * from quest_edificio_superficieestanciasocupadas(ARRAY[1,2,3,5], ''B101'', ''0037'');
- select quest_edificio_superficieestanciasocupadas(ARRAY[1,2,3,5], ''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_superficieestanciasocupadas(tipo character varying, denominacion character varying, edificio character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
 superficie double precision;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT sum(st_area(t.geometria)) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo IN (SELECT codigo FROM todaspersonas) 
             AND substring(t.codigo from 1 for 4) = ' || quote_literal(edificio) || ';' INTO superficie;
 RETURN superficie;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_superficieestanciasocupadas(tipo character varying, denominacion character varying, edificio character varying) IS 'Obtiene la superficie de las estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un en un edificio.
SINTAXIS
SELECT quest_edificio_superficieestanciasocupadas(''crue'',''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_superficieestanciasocupadasbec(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	edificio varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        edificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM becarios)
	AND actividad = $1
	AND substring(codigo from 1 for 4) = edificio;
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_edificio_superficieestanciasocupadasbec(integer, character varying) IS 'Obtiene la superficie de las estancias de un edificio ocupadas por becarios de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_edificio_superficieestanciasocupadasbecarios(8, ''0037'');
- SELECT quest_edificio_superficieestanciasocupadasbecarios(8, ''0037'');';

CREATE FUNCTION quest_edificio_superficieestanciasocupadasbec(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	edificio varchar;
	superficie float8;
BEGIN
        uso := $1;
        edificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM becarios)
	AND actividad = ANY ($1)
	AND substring(codigo from 1 for 4) = edificio;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_superficieestanciasocupadasbec(integer[], character varying) IS 'Obtiene la superficie de las estancias de un edificio ocupadas por becarios de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_edificio_superficieestanciasocupadasbecarios(ARRAY[8,9,16],''0037'');
- SELECT quest_edificio_superficieestanciasocupadasbecarios(ARRAY[8,9,16],''0037'');';

CREATE FUNCTION quest_edificio_superficieestanciasocupadasbec(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM becarios)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 1 for 4) = upper($3)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_edificio_superficieestanciasocupadasbec(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por becarios de una lista de actividades SIGUANET y en un edificio de la Universidad. 
SINTAXIS:
- SELECT * FROM quest_edificio_superficieestanciasocupadasbec(ARRAY[8,9,16], ''B101'', ''0037'');
- SELECT quest_edificio_superficieestanciasocupadasbec(ARRAY[8,9,16], ''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_superficieestanciasocupadasext(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	edificio varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        edificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalexternos)
	AND actividad = $1
	AND substring(codigo from 1 for 4) = edificio;
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_edificio_superficieestanciasocupadasext(integer, character varying) IS 'Obtiene la superficie de las estancias de un edificio ocupadas por externos de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_edificio_superficieestanciasocupadasexternos(8, ''0037'');
- SELECT quest_edificio_superficieestanciasocupadasexternos(8, ''0037'');';

CREATE FUNCTION quest_edificio_superficieestanciasocupadasext(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	edificio varchar;
	superficie float8;
BEGIN
        uso := $1;
        edificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalexternos)
	AND actividad = ANY ($1)
	AND substring(codigo from 1 for 4) = edificio;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_superficieestanciasocupadasext(integer[], character varying) IS 'Obtiene la superficie de las estancias de un edificio ocupadas por externos de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_edificio_superficieestanciasocupadasexternos(ARRAY[8,9,16],''0037'');
- SELECT quest_edificio_superficieestanciasocupadasexternos(ARRAY[8,9,16],''0037'');';

CREATE FUNCTION quest_edificio_superficieestanciasocupadasext(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalexternos)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 1 for 4) = upper($3)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_edificio_superficieestanciasocupadasext(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por externos de una lista de actividades SIGUANET y en un edificio de la Universidad. 
SINTAXIS:
- SELECT * FROM quest_edificio_superficieestanciasocupadasext(ARRAY[8,9,16], ''B101'', ''0037'');
- SELECT quest_edificio_superficieestanciasocupadasext(ARRAY[8,9,16], ''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_superficieestanciasocupadaspas(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	edificio varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        edificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpas)
	AND actividad = $1
	AND substring(codigo from 1 for 4) = edificio;
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_edificio_superficieestanciasocupadaspas(integer, character varying) IS 'Obtiene la superficie de las estancias de un edificio ocupadas por PAS de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_edificio_superficieestanciasocupadaspas(8, ''0037'');
- SELECT quest_edificio_superficieestanciasocupadaspas(8, ''0037'');';

CREATE FUNCTION quest_edificio_superficieestanciasocupadaspas(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	edificio varchar;
	superficie float8;
BEGIN
        uso := $1;
        edificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpas)
	AND actividad = ANY ($1)
	AND substring(codigo from 1 for 4) = edificio;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_superficieestanciasocupadaspas(integer[], character varying) IS 'Obtiene la superficie de las estancias de un edificio ocupadas por PAS de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_edificio_superficieestanciasocupadaspas(ARRAY[8,9,16],''0037'');
- SELECT quest_edificio_superficieestanciasocupadaspas(ARRAY[8,9,16],''0037'');';

CREATE FUNCTION quest_edificio_superficieestanciasocupadaspas(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpas)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 1 for 4) = upper($3)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_edificio_superficieestanciasocupadaspas(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por PAS de una lista de actividades SIGUANET y en un edificio de la Universidad. 
SINTAXIS:
- SELECT * FROM quest_edificio_superficieestanciasocupadaspas(ARRAY[8,9,16], ''B101'', ''0037'');
- SELECT quest_edificio_superficieestanciasocupadaspas(ARRAY[8,9,16], ''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_superficieestanciasocupadaspdi(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	edificio varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        edificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpdi)
	AND actividad = $1
	AND substring(codigo from 1 for 4) = edificio;
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_edificio_superficieestanciasocupadaspdi(integer, character varying) IS 'Obtiene la superficie de las estancias de un edificio ocupadas por PDI de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_edificio_superficieestanciasocupadaspdi(8, ''0037'');
- SELECT quest_edificio_superficieestanciasocupadaspdi(8, ''0037'');';

CREATE FUNCTION quest_edificio_superficieestanciasocupadaspdi(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	edificio varchar;
	superficie float8;
BEGIN
        uso := $1;
        edificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpdi)
	AND actividad = ANY ($1)
	AND substring(codigo from 1 for 4) = edificio;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_edificio_superficieestanciasocupadaspdi(integer[], character varying) IS 'Obtiene la superficie de las estancias de un edificio ocupadas por PDI de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_edificio_superficieestanciasocupadaspdi(ARRAY[8,9,16],''0037'');
- SELECT quest_edificio_superficieestanciasocupadaspdi(ARRAY[8,9,16],''0037'');';

CREATE FUNCTION quest_edificio_superficieestanciasocupadaspdi(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpdi)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 1 for 4) = upper($3)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_edificio_superficieestanciasocupadaspdi(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por PDI de una lista de actividades SIGUANET y en un edificio de la Universidad. 
SINTAXIS:
- SELECT * FROM quest_edificio_superficieestanciasocupadaspdi(ARRAY[8,9,16], ''B101'', ''0037'');
- SELECT quest_edificio_superficieestanciasocupadaspdi(ARRAY[8,9,16], ''B101'', ''0037'');';

CREATE FUNCTION quest_edificio_superficieutil(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE a.util = true AND substring(t.codigo from 1 for 4) = upper($1);
$_$;

COMMENT ON FUNCTION quest_edificio_superficieutil(character varying) IS 'Obtiene la superficie útil de un edificio de la Universidad.
Se ejecuta de dos formas:
- select * from quest_edificio_superficieutil(''0037'');
- select quest_edificio_superficieutil(''0037'');
';

CREATE FUNCTION quest_edificio_superficieutil(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) 
	FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE a.util = true 
	AND t.coddpto = upper($1)
	AND substring(t.codigo from 1 for 4) = upper($2);
$_$;

COMMENT ON FUNCTION quest_edificio_superficieutil(character varying, character varying) IS 'Obtiene la superficie útil de un departamento SIGUANET en un edificio de la Universidad.
Se ejecuta de dos formas:
- select * from quest_edificio_superficieutil(''B101'', ''0037'');
- select quest_edificio_superficieutil(''B101'', ''0037'');';

CREATE FUNCTION quest_isadmin()
  RETURNS boolean AS
$BODY$
DECLARE
 isadmin boolean := false;
 rolename text;
 c CURSOR FOR SELECT grouprole FROM quest_adminroles;
BEGIN
 SELECT INTO isadmin usesuper FROM pg_user WHERE usename = user;
 IF NOT isadmin THEN
  OPEN c;
  LOOP
   FETCH c INTO rolename;
   EXIT WHEN NOT FOUND;
   SELECT INTO isadmin pg_has_role (user, rolename, 'MEMBER');
   EXIT WHEN isadmin;
  END LOOP;  
  CLOSE c;
 END IF;
 RETURN isadmin;
END;$BODY$
  LANGUAGE plpgsql STABLE;

COMMENT ON FUNCTION quest_isadmin() IS 'Devuelve TRUE cuando el usuario que realiza la transacción es SUPERUSER o es miembro de alguno de los roles en la tabla quest_adminroles. En caso contrario devuelve FALSE.';

CREATE VIEW quest_nifmd5 AS 
 SELECT personal.nif, md5(personal.nif::text) AS nifmd5
   FROM public.personal;

CREATE VIEW quest_becarios AS
    SELECT CASE WHEN quest_isadmin() THEN a.nif ELSE b.nifmd5 END AS nif, a.codigo, a.apellido1, a.apellido2, a.nombre, a.cod_depto_centro_subunidad, a.cod_puesto FROM (public.becarios a JOIN quest_nifmd5 b ON (((a.nif)::text = (b.nif)::text)));

COMMENT ON VIEW quest_becarios IS 'Vista de la tabla becarios';

CREATE VIEW quest_checklist_personas AS
    SELECT CASE WHEN quest_isadmin() THEN p.nif ELSE e.nifmd5 END AS nif, ((p.apellido1)::text)::character varying AS apellido1, ((p.apellido2)::text)::character varying AS apellido2, ((p.nombre)::text)::character varying AS nombre, v.espas, v.locpas, v.espdi, v.locpdi, v.espdicargo, v.locpdicargo, v.esbecario, v.locbecario, v.esexterno, v.locexterno FROM ((public.personal p JOIN ((((((((SELECT a.nif, false AS espas, false AS locpas, true AS espdi, ((a.codigo)::text <> '0000PB997'::text) AS locpdi, false AS espdicargo, false AS locpdicargo, false AS esbecario, false AS locbecario, false AS esexterno, false AS locexterno FROM (public.personalpdi a JOIN ((((SELECT personalpdi.nif FROM public.personalpdi EXCEPT SELECT personalpdi_cargos.nif FROM public.personalpdi_cargos) EXCEPT SELECT personalpas.nif FROM public.personalpas) EXCEPT SELECT becarios.nif FROM public.becarios) EXCEPT SELECT personalexternos.nif FROM public.personalexternos) b ON (((a.nif)::text = (b.nif)::text))) UNION SELECT a.nif, true AS espas, ((b.codigo)::text <> '0000PB997'::text) AS locpas, true AS espdi, ((a.codigo)::text <> '0000PB997'::text) AS locpdi, false AS espdicargo, false AS locpdicargo, false AS esbecario, false AS locbecario, false AS esexterno, false AS locexterno FROM ((public.personalpdi a JOIN public.personalpas b ON (((a.nif)::text = (b.nif)::text))) JOIN ((((SELECT personalpdi.nif FROM public.personalpdi EXCEPT SELECT personalpdi_cargos.nif FROM public.personalpdi_cargos) INTERSECT SELECT personalpas.nif FROM public.personalpas) EXCEPT SELECT becarios.nif FROM public.becarios) EXCEPT SELECT personalexternos.nif FROM public.personalexternos) c ON (((a.nif)::text = (c.nif)::text)))) UNION SELECT a.nif, false AS espas, false AS locpas, true AS espdi, ((a.codigo)::text <> '0000PB997'::text) AS locpdi, false AS espdicargo, false AS locpdicargo, true AS esbecario, ((b.codigo)::text <> '0000PB997'::text) AS locbecario, false AS esexterno, false AS locexterno FROM ((public.personalpdi a JOIN public.becarios b ON (((a.nif)::text = (b.nif)::text))) JOIN ((((SELECT personalpdi.nif FROM public.personalpdi EXCEPT SELECT personalpdi_cargos.nif FROM public.personalpdi_cargos) EXCEPT SELECT personalpas.nif FROM public.personalpas) INTERSECT SELECT becarios.nif FROM public.becarios) EXCEPT SELECT personalexternos.nif FROM public.personalexternos) c ON (((a.nif)::text = (c.nif)::text)))) UNION SELECT a.nif, false AS espas, false AS locpas, true AS espdi, ((a.codigo)::text <> '0000PB997'::text) AS locpdi, false AS espdicargo, false AS locpdicargo, false AS esbecario, false AS locbecario, true AS esexterno, ((b.codigo)::text <> '0000PB997'::text) AS locexterno FROM ((public.personalpdi a JOIN public.personalexternos b ON (((a.nif)::text = (b.nif)::text))) JOIN ((((SELECT personalpdi.nif FROM public.personalpdi EXCEPT SELECT personalpdi_cargos.nif FROM public.personalpdi_cargos) EXCEPT SELECT personalpas.nif FROM public.personalpas) EXCEPT SELECT becarios.nif FROM public.becarios) INTERSECT SELECT personalexternos.nif FROM public.personalexternos) c ON (((a.nif)::text = (c.nif)::text)))) UNION SELECT a.nif, false AS espas, false AS locpas, true AS espdi, ((a.codigo)::text <> '0000PB997'::text) AS locpdi, true AS espdicargo, ((b.codigo)::text <> '0000PB997'::text) AS locpdicargo, false AS esbecario, false AS locbecario, false AS esexterno, false AS locexterno FROM ((public.personalpdi a JOIN public.personalpdi_cargos b ON (((a.nif)::text = (b.nif)::text))) JOIN ((((SELECT personalpdi.nif FROM public.personalpdi INTERSECT SELECT personalpdi_cargos.nif FROM public.personalpdi_cargos) EXCEPT SELECT personalpas.nif FROM public.personalpas) EXCEPT SELECT becarios.nif FROM public.becarios) EXCEPT SELECT personalexternos.nif FROM public.personalexternos) c ON (((a.nif)::text = (c.nif)::text)))) UNION SELECT a.nif, false AS espas, false AS locpas, true AS espdi, ((a.codigo)::text <> '0000PB997'::text) AS locpdi, true AS espdicargo, ((b.codigo)::text <> '0000PB997'::text) AS locpdicargo, false AS esbecario, false AS locbecario, true AS esexterno, ((c.codigo)::text <> '0000PB997'::text) AS locexterno FROM (((public.personalpdi a JOIN public.personalpdi_cargos b ON (((a.nif)::text = (b.nif)::text))) JOIN public.personalexternos c ON (((a.nif)::text = (c.nif)::text))) JOIN ((((SELECT personalpdi.nif FROM public.personalpdi INTERSECT SELECT personalpdi_cargos.nif FROM public.personalpdi_cargos) EXCEPT SELECT personalpas.nif FROM public.personalpas) EXCEPT SELECT becarios.nif FROM public.becarios) INTERSECT SELECT personalexternos.nif FROM public.personalexternos) d ON (((a.nif)::text = (d.nif)::text)))) UNION SELECT a.nif, true AS espas, ((a.codigo)::text <> '0000PB997'::text) AS locpas, false AS espdi, false AS locpdi, false AS espdicargo, false AS locpdicargo, false AS esbecario, false AS locbecario, false AS esexterno, false AS locexterno FROM (public.personalpas a JOIN (((SELECT personalpas.nif FROM public.personalpas EXCEPT SELECT personalpdi.nif FROM public.personalpdi) EXCEPT SELECT becarios.nif FROM public.becarios) EXCEPT SELECT personalexternos.nif FROM public.personalexternos) b ON (((a.nif)::text = (b.nif)::text)))) UNION SELECT a.nif, false AS espas, false AS locpas, false AS espdi, false AS locpdi, false AS espdicargo, false AS locpdicargo, true AS esbecario, ((a.codigo)::text <> '0000PB997'::text) AS locbecario, false AS esexterno, false AS locexterno FROM (public.becarios a JOIN (((SELECT becarios.nif FROM public.becarios EXCEPT SELECT personalpdi.nif FROM public.personalpdi) EXCEPT SELECT personalpas.nif FROM public.personalpas) EXCEPT SELECT personalexternos.nif FROM public.personalexternos) b ON (((a.nif)::text = (b.nif)::text)))) UNION SELECT a.nif, false AS espas, false AS locpas, false AS espdi, false AS locpdi, false AS espdicargo, false AS locpdicargo, false AS esbecario, false AS locbecario, true AS esexterno, ((a.codigo)::text <> '0000PB997'::text) AS locexterno FROM (public.personalexternos a JOIN ((((SELECT personalexternos.nif FROM public.personalexternos EXCEPT SELECT personalpdi.nif FROM public.personalpdi) EXCEPT SELECT personalpdi_cargos.nif FROM public.personalpdi_cargos) EXCEPT SELECT personalpas.nif FROM public.personalpas) EXCEPT SELECT becarios.nif FROM public.becarios) b ON (((a.nif)::text = (b.nif)::text)))) v ON (((p.nif)::text = (v.nif)::text))) JOIN quest_nifmd5 e ON (((p.nif)::text = (e.nif)::text))) ORDER BY p.apellido1, p.apellido2, p.nombre;

COMMENT ON VIEW quest_checklist_personas IS 'Permite crear objetos de tipo checklist de persona: para cada registro obtenemos un conjunto de campos booleanos que indican si la persona pertenece a la categoría PAS, PDI, PDI_CARGO, BECARIO o EXTERNO, y si tiene ubicación asignada para cada categoría a la que pertenezca.
Combinaciones contempladas:
- PAS (EXCLUSIVO)
- PDI (EXCLUSIVO)
- PDI_CARGO (EXCLUSIVO)
- BECARIO (EXCLUSIVO)
- EXTERNO (EXCLUSIVO)
- PDI + PAS
- PDI + BECARIO
- PDI + EXTERNO
- PDI_CARGO + EXTERNO

En el futuro habrá que contemplar otras combinaciones.';

CREATE VIEW quest_personalexternos AS
    SELECT CASE WHEN quest_isadmin() THEN a.nif ELSE b.nifmd5 END AS nif, a.apellido1, a.apellido2, a.nombre, a.codigo, a.cod_puesto, a.cod_dpto_sigua FROM (public.personalexternos a JOIN quest_nifmd5 b ON (((a.nif)::text = (b.nif)::text)));

COMMENT ON VIEW quest_personalexternos IS 'Vista de la tabla personalexternos para nif encriptado';

CREATE VIEW quest_personalpas AS
    SELECT CASE WHEN quest_isadmin() THEN a.nif ELSE b.nifmd5 END AS nif, a.codigo, a.cod_puesto, a.cod_unidad, a.cod_sub, a.cod_centro FROM (public.personalpas a JOIN quest_nifmd5 b ON (((a.nif)::text = (b.nif)::text)));

COMMENT ON VIEW quest_personalpas IS 'Vista de la tabla personalpas para nif encriptado';

CREATE VIEW quest_personalpdi AS
    SELECT CASE WHEN quest_isadmin() THEN a.nif ELSE b.nifmd5 END AS nif, a.codigo, a.cod_puesto, a.cod_centro, a.cod_depto FROM (public.personalpdi a JOIN quest_nifmd5 b ON (((a.nif)::text = (b.nif)::text)));

COMMENT ON VIEW quest_personalpdi IS 'Vista de la tabla personalpdi para nif encriptado';

CREATE VIEW quest_personalpdi_cargos AS
    SELECT CASE WHEN quest_isadmin() THEN a.nif ELSE b.nifmd5 END AS nif, a.codigo, a.cod_cargo FROM (public.personalpdi_cargos a JOIN quest_nifmd5 b ON (((a.nif)::text = (b.nif)::text)));

COMMENT ON VIEW quest_personalpdi_cargos IS 'Vista de la tabla personalpdi_cargos para nif encriptado';

CREATE VIEW quest_personas AS
    (((SELECT quest_personalpas.nif, quest_personalpas.codigo, 'personalpas'::text AS reftbl FROM quest_personalpas UNION SELECT quest_personalpdi.nif, quest_personalpdi.codigo, 'personalpdi'::text AS reftbl FROM quest_personalpdi) UNION SELECT quest_personalpdi_cargos.nif, quest_personalpdi_cargos.codigo, 'personalpdi_cargos'::text AS reftbl FROM quest_personalpdi_cargos) UNION SELECT quest_becarios.nif, quest_becarios.codigo, 'becarios'::text AS reftbl FROM quest_becarios) UNION SELECT quest_personalexternos.nif, quest_personalexternos.codigo, 'personalexternos'::text AS reftbl FROM quest_personalexternos;

COMMENT ON VIEW quest_personas IS 'Permite crear objetos de tipo persona y proporciona información acerca de la tabla de la que procede el registro de ubicación de la persona.';

CREATE VIEW quest_ubicaciones AS
    SELECT DISTINCT p.nif, p.apellido1, p.apellido2, p.nombre, p.espas, p.locpas, p.espdi, p.locpdi, p.espdicargo, p.locpdicargo, p.esbecario, p.locbecario, p.esexterno, p.locexterno, 0 AS gid, te.codigo, public.st_astext(te.geometria) AS wkt, public.st_srid(te.geometria) AS srid, "substring"((te.codigo)::text, 1, 6) AS codplantaedif, "substring"((te.codigo)::text, 5, 2) AS enumplanta, (((e.txt_edificio)::text || ' '::text) || "substring"((te.codigo)::text, 5, 2)) AS denoplanta, ((e.cod_zona)::text || (e.cod_edificio)::text) AS codedificio, e.txt_edificio AS denoedificio, z.cod_zona AS codzona, z.txt_zona AS denozona, te.actividad, a.txt_actividad AS denoactividad, a.activresum AS denogrupo, a.crue AS denocrue, a.u21 AS denou21, te.coddpto, ds.txt AS denodpto, ds.es_centro, ds.es_dpto, ds.es_unidad, te.denominaci AS denoestancia, te.observacio AS observaestancia, tp.reftbl FROM public.todasestancias te, public.edificios e, public.zonas z, public.actividades a, quest_departamentos ds, quest_personas tp, quest_checklist_personas p WHERE ((((((((te.codigo)::text <> '0000PB997'::text) AND ("substring"((te.codigo)::text, 1, 4) = ((e.cod_zona)::text || (e.cod_edificio)::text))) AND ("substring"((te.codigo)::text, 1, 2) = (z.cod_zona)::text)) AND (te.actividad = a.codactividad)) AND ((te.coddpto)::text = (ds.cod)::text)) AND ((te.codigo)::text = (tp.codigo)::text)) AND ((tp.nif)::text = (p.nif)::text)) ORDER BY "substring"((te.codigo)::text, 1, 6), p.apellido1, p.apellido2, p.nif, te.codigo, public.st_astext(te.geometria), public.st_srid(te.geometria), "substring"((te.codigo)::text, 5, 2), (((e.txt_edificio)::text || ' '::text) || "substring"((te.codigo)::text, 5, 2)), ((e.cod_zona)::text || (e.cod_edificio)::text), e.txt_edificio, z.cod_zona, z.txt_zona, te.actividad, a.txt_actividad, a.activresum, a.crue, a.u21, te.coddpto, ds.txt, ds.es_centro, ds.es_dpto, ds.es_unidad, te.denominaci, te.observacio, p.nombre, p.espas, p.locpas, p.espdi, p.locpdi, p.espdicargo, p.locpdicargo, p.esbecario, p.locbecario, p.esexterno, p.locexterno, 0::integer, tp.reftbl;

COMMENT ON VIEW quest_ubicaciones IS 'Permite crear objetos de tipo ubicación, es decir, las asociaciones persona/estancia, excepto las personas no ubicadas.';

CREATE FUNCTION quest_edificio_ubicaciones(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE codedificio = $1;
$_$;

COMMENT ON FUNCTION quest_edificio_ubicaciones(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de un edificio de la Universidad.
SINTAXIS
- SELECT * FROM quest_edificio_ubicaciones(''0011'')';

CREATE FUNCTION quest_edificio_ubicaciones(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT DISTINCT v.* FROM quest_ubicaciones v
	JOIN quest_personas2 p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE p.cod_depto = upper($1)
	AND v.codedificio = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_edificio_ubicaciones(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de las personas adscritas a un departamento SIGUANET en un edificio de la Universidad.
SINTAXIS
- SELECT * FROM quest_edificio_ubicaciones(''B101'', ''0037'')';

CREATE FUNCTION quest_edificio_ubicaciones(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones WHERE actividad = $1 AND substring(codigo from 1 for 4) = $2;
$_$;

COMMENT ON FUNCTION quest_edificio_ubicaciones(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en un edificio.
SINTAXIS
- SELECT * FROM quest_edificio_ubicaciones(7, ''0037'')';

CREATE FUNCTION quest_edificio_ubicaciones(tipo character varying, denominacion character varying, edificio character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND substring(codigo FROM 1 FOR 4) = ' || quote_literal(edificio) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_ubicaciones(tipo character varying, denominacion character varying, edificio character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para las personas ubicadas en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad, en un edificio.
SINTAXIS:
- SELECT * FROM quest_edificio_ubicaciones(''crue'', ''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_ubicacionesbecarios(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones
   WHERE esbecario = true
   AND locbecario = true
   AND reftbl = 'becarios' 
   AND (actividad <= 50 OR actividad = 99)
   AND codedificio = $1;
$_$;

COMMENT ON FUNCTION quest_edificio_ubicacionesbecarios(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de un edificio de la Universidad.
La condición where matiza que sólo devuelve estancias con BECARIOS
SINTAXIS
- SELECT * FROM quest_edificio_ubicacionesbecarios(''0011'')';

CREATE FUNCTION quest_edificio_ubicacionesbecarios(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_becarios p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.esbecario = true 
	AND v.locbecario = true
	AND reftbl = 'becarios' 
	AND p.cod_depto_centro_subunidad = upper($1)
	AND v.codedificio = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_edificio_ubicacionesbecarios(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociación becario/estancia de los becarios adscritos a un departamento SIGUANET ubicados en un edificio de la Universidad.
SINTAXIS
- SELECT * FROM quest_edificio_ubicacionesbecarios(''B101'', ''0037'')';

CREATE FUNCTION quest_edificio_ubicacionesbecarios(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 AND substring(codigo from 1 for 4) = $2 
   AND esbecario = true AND locbecario = true
   AND reftbl = 'becarios';
$_$;

COMMENT ON FUNCTION quest_edificio_ubicacionesbecarios(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion becario/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en un edificio.
SINTAXIS
- SELECT * FROM quest_edificio_ubicacionesbecarios(7, ''0018'')';

CREATE FUNCTION quest_edificio_ubicacionesbecarios(tipo character varying, denominacion character varying, edificio character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND esbecario = true 
                       AND locbecario = true
                       AND reftbl = ''becarios''
                       AND substring(codigo FROM 1 FOR 4) = ' || quote_literal(edificio) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_ubicacionesbecarios(tipo character varying, denominacion character varying, edificio character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los BECARIOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un edificio.
SINTAXIS:
- SELECT * FROM quest_edificio_ubicacionesbecarios(''crue'', ''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_ubicacionescargos(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE espdicargo = true
   AND locpdicargo = true
   AND reftbl = 'personalpdi_cargos' 
   AND (actividad <= 50 OR actividad = 99)
   AND codedificio = $1;
$_$;

COMMENT ON FUNCTION quest_edificio_ubicacionescargos(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de un edificio de la Universidad.
La condición where matiza que sólo devuelve estancias con CARGOS
SINTAXIS
- SELECT * FROM quest_edificio_ubicacionescargos(''0011'')';

CREATE FUNCTION quest_edificio_ubicacionescargos(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalpdi_cargos p ON v.nif = p.nif AND v.codigo = p.codigo
	JOIN cargos_dpto cd ON p.cod_cargo = cd.cod_cargo
	WHERE v.espdicargo = true 
	AND v.locpdicargo = true
	AND reftbl = 'personalpdi_cargos' 
	AND cd.coddpto = upper($1)
	AND v.codedificio = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_edificio_ubicacionescargos(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociación pdicargo/estancia de los cargos adscritos a un departamento SIGUANET ubicados en un edificio de la Universidad.
SINTAXIS
- SELECT * FROM quest_edificio_ubicacionescargos(''B101'', ''0037'')';

CREATE FUNCTION quest_edificio_ubicacionesexternos(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones
   WHERE esexterno = true
   AND locexterno = true
   AND reftbl = 'personalexternos'   
   AND (actividad <= 50 OR actividad = 99)
   AND codedificio = $1;
$_$;

COMMENT ON FUNCTION quest_edificio_ubicacionesexternos(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de un edificio de la Universidad.
La condición where matiza que sólo devuelve estancias DE EXTERNOS
SINTAXIS
- SELECT * FROM quest_edificio_ubicacionesexternos(''0011'')';

CREATE FUNCTION quest_edificio_ubicacionesexternos(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalexternos p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.esexterno = true 
	AND v.locexterno = true
	AND v.reftbl = 'personalexternos'   
	AND p.cod_dpto_sigua = upper($1)
	AND v.codedificio = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_edificio_ubicacionesexternos(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion externo/estancia de los externos adscritos a un departamento SIGUANET ubicados en un edificio de la Universidad.
SINTAXIS
- SELECT * FROM quest_edificio_ubicacionesexternos(''B101'', ''0037'')';

CREATE FUNCTION quest_edificio_ubicacionesexternos(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 AND substring(codigo from 1 for 4) = $2 
   AND esexterno = true AND locexterno = true
   AND reftbl = 'personalexternos';
$_$;

COMMENT ON FUNCTION quest_edificio_ubicacionesexternos(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion externo/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en un edificio.
SINTAXIS
- SELECT * FROM quest_edificio_ubicacionesexternos(7, ''0018'')';

CREATE FUNCTION quest_edificio_ubicacionesexternos(tipo character varying, denominacion character varying, edificio character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND esexterno = true 
                       AND locexterno = true
                       AND reftbl = ''personalexternos''
                       AND substring(codigo FROM 1 FOR 4) = ' || quote_literal(edificio) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_ubicacionesexternos(tipo character varying, denominacion character varying, edificio character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los empleados EXTERNOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un edificio.
SINTAXIS:
- SELECT * FROM quest_edificio_ubicacionesexternos(''crue'', ''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_ubicacionespas(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_ubicaciones 
	WHERE espas = true
	AND locpas = true
	AND reftbl = 'personalpas'
	AND (actividad <= 50 OR actividad = 99)
	AND codedificio = $1;
$_$;

COMMENT ON FUNCTION quest_edificio_ubicacionespas(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de una planta de un edificio de la Universidad.
La condición where matiza que sólo devuelve estancias con pas
SINTAXIS
- SELECT * FROM quest_plantaedificio_ubicacionespas(''0011PB'')';

CREATE FUNCTION quest_edificio_ubicacionespas(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalpas p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.espas = true 
	AND v.locpas = true
	AND v.reftbl = 'personalpas'
	AND p.cod_unidad = upper($1)
	AND v.codedificio = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_edificio_ubicacionespas(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion pas/estancia de los pas adscritos a un departamento SIGUANET ubicados en un edificio de la Universidad.
La condición where matiza que sólo devuelve estancias con pas
SINTAXIS
- SELECT * FROM quest_edificio_ubicacionespas(''B101'', ''0037'')';

CREATE FUNCTION quest_edificio_ubicacionespas(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 AND substring(codigo from 1 for 4) = $2 
   AND espas = true AND locpas = true
   AND reftbl = 'personalpas';
$_$;

COMMENT ON FUNCTION quest_edificio_ubicacionespas(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion PAS/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en un edificio.
SINTAXIS
- SELECT * FROM quest_edificio_ubicacionespas(7, ''0018'')';

CREATE FUNCTION quest_edificio_ubicacionespas(tipo character varying, denominacion character varying, edificio character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND espas = true 
                       AND locpas = true
                       AND reftbl = ''personalpas''
                       AND substring(codigo FROM 1 FOR 4) = ' || quote_literal(edificio) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_ubicacionespas(tipo character varying, denominacion character varying, edificio character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los PAS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un edificio.
SINTAXIS:
- SELECT * FROM quest_edificio_ubicacionespas(''crue'', ''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_edificio_ubicacionespdi(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE espdi = true 
   AND locpdi = true
   AND reftbl = 'personalpdi'
   AND (actividad <= 50 OR actividad = 99)
   AND codedificio = $1;
$_$;

COMMENT ON FUNCTION quest_edificio_ubicacionespdi(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de un edificio de la Universidad.
La condición where matiza que sólo devuelve estancias con PDI
SINTAXIS
- SELECT * FROM quest_edificio_ubicacionespdi(''0011'')';

CREATE FUNCTION quest_edificio_ubicacionespdi(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalpdi p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.espdi = true 
	AND v.locpdi = true
	AND v.reftbl = 'personalpdi'	
	AND p.cod_depto = upper($1)
	AND v.codedificio = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_edificio_ubicacionespdi(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociación pdi/estancia de los pdi adscritos a un departamento SIGUANET ubicados en un edificio de la Universidad.
SINTAXIS
- SELECT * FROM quest_edificio_ubicacionespdi(''B101'', ''0037'')';

CREATE FUNCTION quest_edificio_ubicacionespdi(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 AND substring(codigo from 1 for 4) = $2 
   AND espdi = true AND locpdi = true
   AND reftbl = 'personalpdi';
$_$;

COMMENT ON FUNCTION quest_edificio_ubicacionespdi(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion pdi/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en un edificio.
SINTAXIS
- SELECT * FROM quest_edificio_ubicacionespdi(7, ''0018'')';

CREATE FUNCTION quest_edificio_ubicacionespdi(tipo character varying, denominacion character varying, edificio character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND espdi = true 
                       AND locpdi = true
                       AND reftbl = ''personalpdi''
                       AND substring(codigo FROM 1 FOR 4) = ' || quote_literal(edificio) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_edificio_ubicacionespdi(tipo character varying, denominacion character varying, edificio character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los PDI ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un edificio.
SINTAXIS:
- SELECT * FROM quest_edificio_ubicacionespdi(''crue'', ''DOCENCIA'', ''0037'');';

CREATE FUNCTION quest_estancia_numpersonas(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(DISTINCT nif) FROM todaspersonas WHERE codigo = upper($1);
$_$;

COMMENT ON FUNCTION quest_estancia_numpersonas(character varying) IS 'Obtiene el nº de personas en una estancia de la Universidad.
Se ejecuta de dos formas:
- select * from quest_estancia_numpersonas(''0037P1015'');
- select quest_estancia_numpersonas(''0037P1015'');
';

CREATE FUNCTION quest_estancia_obtenerpersonas(character varying) RETURNS SETOF quest_checklist_personas
    LANGUAGE sql
    AS $_$
  SELECT DISTINCT v.* FROM quest_checklist_personas v JOIN quest_personas tp ON v.nif = tp.nif
  WHERE tp.codigo = upper($1);
$_$;

COMMENT ON FUNCTION quest_estancia_obtenerpersonas(character varying) IS 'Obtiene la relación de personas en una estancia de la Universidad.
Se ejecuta de dos formas:
- select * from quest_estancia_obtenerpersonas(''0037P1015'');
- select quest_estancia_obtenerpersonas(''0037P1015'');
';

CREATE FUNCTION quest_plantaedificio_densidad(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantaedificio_superficieestanciasocupadas(ARRAY[4,5,7,8,9,16], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE actividad = ANY (ARRAY[4,5,7,8,9,16])
										       AND substring(codigo from 1 for 6) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_plantaedificio_densidad(character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en un edificio y una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_densidad(''0037PB'');
- select quest_plantaedificio_densidad(''0037PB'');';

CREATE FUNCTION quest_plantaedificio_densidad(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantaedificio_superficieestanciasocupadas(ARRAY[4,5,7,8,9,16], $1, $2) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE actividad = ANY (ARRAY[4,5,7,8,9,16])
						     AND coddpto = upper($1)
						     AND substring(codigo from 1 for 6) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_plantaedificio_densidad(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en un departamento SIGUANET y en una planta de un edificio de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_densidad(''B101'', ''0037PB'');
- select quest_plantaedificio_densidad(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_densidad(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_plantaedificio_superficieestanciasocupadas($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM todaspersonas WHERE codigo IN (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 6) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_densidad(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en las estancias de una determinada actividad SIGUANET para una planta de un edificio. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_densidad(4, ''0037PB'');
- select quest_plantaedificio_densidad(8, ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_densidad(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  SELECT quest_plantaedificio_superficieestanciasocupadas(tipo, denominacion, plantaedificio) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM todaspersonas WHERE codigo IN
            (SELECT codigo FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
          '   AND substring(codigo FROM 1 FOR 6) = ' || quote_literal(upper(plantaedificio)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;   
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_densidad(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad, en una planta de edificio. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_plantaedificio_densidad(''crue'', ''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_densidadbecarios(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantaedificio_superficieestanciasocupadasbec(ARRAY[4,5,7], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM becarios)
										       AND actividad = ANY (ARRAY[4,5,7])
										       AND substring(codigo from 1 for 6) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_plantaedificio_densidadbecarios(character varying) IS 'Obtiene los m2 de espacio de trabajo por becario en un edificio y una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_densidadbecarios(''0037PB'');
- select quest_plantaedificio_densidadbecarios(''0037PB'');';

CREATE FUNCTION quest_plantaedificio_densidadbecarios(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantaedificio_superficieestanciasocupadasbec(ARRAY[4,5,7], $1, $2) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM becarios)
						     AND actividad = ANY (ARRAY[4,5,7])
						     AND coddpto = upper($1)
						     AND substring(codigo from 1 for 6) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_plantaedificio_densidadbecarios(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por becarios en un departamento SIGUANET y en una planta de un edificio de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_densidadbecarios(''B101'', ''0037PB'');
- select quest_plantaedificio_densidadbecarios(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_densidadbecarios(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_plantaedificio_superficieestanciasocupadasbec($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM becarios WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM becarios)
                                           AND actividad = $1
                                           AND substring(codigo from 1 for 6) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_densidadbecarios(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por becario en las estancias de una determinada actividad SIGUANET para una planta de un edificio. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_densidadbecarios(4, ''0018PB'');
- select quest_plantaedificio_densidadbecarios(8, ''0018PB'');';

CREATE FUNCTION quest_plantaedificio_densidadbecarios(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_plantaedificio_superficieestanciasocupadasbec(actlist, plantaedificio) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM becarios WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 6) = ' || quote_literal(upper(plantaedificio)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_densidadbecarios(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene los m2 de espacio de trabajo por BECARIO en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_plantaedificio_densidadbecarios(''crue'', ''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_densidadexternos(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantaedificio_superficieestanciasocupadasext(ARRAY[7,8], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalexternos)
										       AND actividad = ANY (ARRAY[7,8])
										       AND substring(codigo from 1 for 6) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_plantaedificio_densidadexternos(character varying) IS 'Obtiene los m2 de espacio de trabajo por externo en un edificio y una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_densidadexternos(''0037PB'');
- select quest_plantaedificio_densidadexternos(''0037PB'');';

CREATE FUNCTION quest_plantaedificio_densidadexternos(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantaedificio_superficieestanciasocupadasext(ARRAY[7,8], $1, $2) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM personalexternos)
						     AND actividad = ANY (ARRAY[7,8])
						     AND coddpto = upper($1)
						     AND substring(codigo from 1 for 6) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_plantaedificio_densidadexternos(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por externos en un departamento SIGUANET y en una planta de un edificio de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_densidadexternos(''B101'', ''0037PB'');
- select quest_plantaedificio_densidadexternos(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_densidadexternos(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_plantaedificio_superficieestanciasocupadasext($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM personalexternos WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalexternos)
                                           AND actividad = $1
                                           AND substring(codigo from 1 for 6) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_densidadexternos(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por externo en las estancias de una determinada actividad SIGUANET para una planta de un edificio. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_densidadexternos(4, ''0018PB'');
- select quest_plantaedificio_densidadexternos(8, ''0018PB'');';

CREATE FUNCTION quest_plantaedificio_densidadexternos(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_plantaedificio_superficieestanciasocupadasext(actlist, plantaedificio) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM personalexternos WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 6) = ' || quote_literal(upper(plantaedificio)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_densidadexternos(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado EXTERNO en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_plantaedificio_densidadexternos(''crue'', ''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_densidadpas(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantaedificio_superficieestanciasocupadaspas(ARRAY[4,5,8,9,16], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpas)
										       AND actividad = ANY (ARRAY[4,5,8,9,16])
										       AND substring(codigo from 1 for 6) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_plantaedificio_densidadpas(character varying) IS 'Obtiene los m2 de espacio de trabajo por PAS en un edificio y una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_densidadpas(''0037PB'');
- select quest_plantaedificio_densidadpas(''0037PB'');';

CREATE FUNCTION quest_plantaedificio_densidadpas(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantaedificio_superficieestanciasocupadaspas(ARRAY[4,5,8,9,16], $1, $2) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM personalpas)
						     AND actividad = ANY (ARRAY[4,5,8,9,16])
						     AND coddpto = upper($1)
						     AND substring(codigo from 1 for 6) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_plantaedificio_densidadpas(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por PAS en un departamento SIGUANET y en una planta de un edificio de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_densidadpas(''B101'', ''0037PB'');
- select quest_plantaedificio_densidadpas(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_densidadpas(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_plantaedificio_superficieestanciasocupadaspas($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM personalpas WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpas)
                                           AND actividad = $1
                                           AND substring(codigo from 1 for 6) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_densidadpas(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado PAS en las estancias de una determinada actividad SIGUANET para una planta de un edificio. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_densidadpas(4, ''0018PB'');
- select quest_plantaedificio_densidadpas(8, ''0018PB'');';

CREATE FUNCTION quest_plantaedificio_densidadpas(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_plantaedificio_superficieestanciasocupadaspas(actlist, plantaedificio) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM personalpas WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 6) = ' || quote_literal(upper(plantaedificio)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_densidadpas(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado tipo PAS en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_plantaedificio_densidadpas(''crue'', ''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_densidadpdi(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantaedificio_superficieestanciasocupadaspdi(ARRAY[4,5,7], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpdi)
										       AND actividad = ANY (ARRAY[4,5,7])
										       AND substring(codigo from 1 for 6) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_plantaedificio_densidadpdi(character varying) IS 'Obtiene los m2 de espacio de trabajo por PDI en un edificio y una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_densidadpdi(''0037PB'');
- select quest_plantaedificio_densidadpdi(''0037PB'');';

CREATE FUNCTION quest_plantaedificio_densidadpdi(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantaedificio_superficieestanciasocupadaspdi(ARRAY[4,5,7], $1, $2) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM personalpdi)
						     AND actividad = ANY (ARRAY[4,5,7])
						     AND coddpto = upper($1)
						     AND substring(codigo from 1 for 6) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_plantaedificio_densidadpdi(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por PDI en un departamento SIGUANET y en una planta de un edificio de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_densidadpdi(''B101'', ''0037PB'');
- select quest_plantaedificio_densidadpdi(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_densidadpdi(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_plantaedificio_superficieestanciasocupadaspdi($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM personalpdi WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpdi)
                                           AND actividad = $1
                                           AND substring(codigo from 1 for 6) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_densidadpdi(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado pdi en las estancias de una determinada actividad SIGUANET para una planta de un edificio. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_densidadpdi(4, ''0018PB'');
- select quest_plantaedificio_densidadpdi(8, ''0018PB'');';

CREATE FUNCTION quest_plantaedificio_densidadpdi(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_plantaedificio_superficieestanciasocupadaspdi(actlist, plantaedificio) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM personalpdi WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 6) = ' || quote_literal(upper(plantaedificio)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_densidadpdi(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado tipo PDI en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_plantaedificio_densidadpdi(''crue'', ''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_estadisticadepartamentossigua(character varying) RETURNS SETOF record
    LANGUAGE plpgsql
    AS $_$
DECLARE
   micursor refcursor;
   fila record;
BEGIN
  OPEN micursor FOR
	SELECT e.coddpto AS cod, d.txt_dpto_sigua AS txt, count(e.codigo) AS conteo, sum(st_area(e.geometria)) AS superficie
	FROM todasestancias e
	JOIN departamentossigua d ON e.coddpto = d.cod_dpto_sigua
	WHERE substring(e.codigo FROM 1 FOR 6) = upper($1)
	AND e.coddpto != '07.018'
	GROUP BY e.coddpto, d.txt_dpto_sigua;
  FETCH micursor INTO fila;
  WHILE FOUND LOOP
    RETURN NEXT fila;
    FETCH micursor INTO fila;
  END LOOP;
  RETURN;

END
$_$;

COMMENT ON FUNCTION quest_plantaedificio_estadisticadepartamentossigua(character varying) IS 'Obtiene un listado de los departamentos SIGUANET con estancias adscritas en una planta de edificio 
Se ejecuta de la siguiente manera:
SELECT * FROM quest_plantaedificio_estadisticadepartamentossigua(''0037P1'') AS (cod varchar, txt varchar, superficie float8);';

CREATE FUNCTION quest_plantaedificio_numadmonnoocupados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND substring(codigo from 1 for 6) = upper($1);
$_$;

CREATE FUNCTION quest_plantaedificio_numadmonnoocupados(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    plantaedificio varchar;    
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   plantaedificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 6) = plantaedificio;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numadmonnoocupados(character varying, character varying) IS 'Obtiene el nº de administraciones no ocupadas de un departamento SIGUANET en una planta de un edificio de la Universidad. 
SINTAXIS:
- select * from quest_plantaedificio_numadmonnoocupados(''B101'', ''0037PB'');
- select quest_plantaedificio_numadmonnoocupados(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numadmonocupados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND substring(codigo from 1 for 6) = upper($1);
$_$;

CREATE FUNCTION quest_plantaedificio_numadmonocupados(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    plantaedificio varchar;    
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   plantaedificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 6) = plantaedificio;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numadmonocupados(character varying, character varying) IS 'Obtiene el nº de administraciones ocupadas de un departamento SIGUANET en una planta de un edificio de la Universidad. 
SINTAXIS:
- select * from quest_plantaedificio_numadmonocupados(''B101'', ''0037PB'');
- select quest_plantaedificio_numadmonocupados(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numbecarios(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM becarios WHERE substring(codigo from 1 for 6) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numbecarios(character varying) IS 'Obtiene el nº de becarios en un edificio y una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantaedificio_numbecarios(''0037PB'');
- select quest_plantaedificio_numbecarios(''0037PB'');
';

CREATE FUNCTION quest_plantaedificio_numbecarios(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM becarios
	WHERE cod_depto_centro_subunidad = upper($1)
	AND substring(codigo from 1 for 6) = upper($2)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numbecarios(character varying, character varying) IS 'Obtiene el total de becarios de un departamento SIGUANET en una planta de un edificio de la Universidad. 
Se ejecuta de dos formas:
- SELECT * FROM quest_plantaedificio_numbecarios(''B101'', ''0037PB'');
- SELECT quest_plantaedificio_numbecarios(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numbecarios(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM becarios WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 6) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_plantaedificio_numbecarios(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM becarios WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 6) = ' || quote_literal(upper(plantaedificio)) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_numbecarios(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene el nº de BECARIOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio.
SINTAXIS:
- SELECT quest_plantaedificio_numbecarios(''crue'', ''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numdespachosnoocupados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND substring(codigo from 1 for 6) = upper($1);
$_$;

CREATE FUNCTION quest_plantaedificio_numdespachosnoocupados(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    plantaedificio varchar;    
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   plantaedificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 6) = plantaedificio;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numdespachosnoocupados(character varying, character varying) IS 'Obtiene el nº de despachos no ocupados de un departamento SIGUANET en una planta de un edificio de la Universidad. 
SINTAXIS:
- select * from quest_plantaedificio_numdespachosnoocupados(''B101'', ''0037PB'');
- select quest_plantaedificio_numdespachosnoocupados(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numdespachosocupados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND substring(codigo from 1 for 6) = upper($1);
$_$;

CREATE FUNCTION quest_plantaedificio_numdespachosocupados(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    plantaedificio varchar;
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   plantaedificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 6) = plantaedificio;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numdespachosocupados(character varying, character varying) IS 'Obtiene el nº de despachos ocupados de un departamento SIGUANET en una planta de un edificio de la Universidad. 
SINTAXIS:
- select * from quest_plantaedificio_numdespachosocupados(''B101'', ''0037PB'');
- select quest_plantaedificio_numdespachosocupados(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numestancias(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(codigo) FROM 
	(SELECT codigo from todasestancias where substring(codigo from 1 for 6) = upper($1) group by codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numestancias(character varying) IS 'Obtiene el nº de estancias en un edificio y una planta de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_numestancias(''0037P1'') ;
- select quest_plantaedificio_numestancias(''0037P1'');
';

CREATE FUNCTION quest_plantaedificio_numestancias(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    cuenta int8;
    adscripcion varchar;
    plantaedificio varchar;    
BEGIN
   adscripcion := upper($1);
   plantaedificio := upper($2);   
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

	SELECT count(codigo) INTO cuenta FROM 
	(SELECT t.codigo FROM todasestancias t, departamentossigua d 
	WHERE t.coddpto = d.cod_dpto_sigua
	AND t.coddpto = adscripcion 
	AND substring(t.codigo from 1 for 6) = plantaedificio
	GROUP BY t.codigo) AS foo;

RETURN cuenta;

END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numestancias(character varying, character varying) IS 'Obtiene el nº de estancias de un departamento SIGUANET en una planta de un edificio de la Universidad
SINTAXIS:
- SELECT quest_plantaedificio_numestancias(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numestancias(integer, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    cuenta int8;

BEGIN
   IF $1 NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', $1;
   ELSE
	SELECT count(codigo) INTO cuenta FROM 
       (SELECT codigo from todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 6) = upper($2) GROUP BY codigo) AS foo;
      RETURN cuenta;
   END IF;
   
END
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numestancias(integer, character varying) IS 'Obtiene el nº de estancias de una determinada actividad SIGUANET en un edificio y una planta de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_numestancias(7, ''0037PB'');
- select quest_plantaedificio_numestancias(8, ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numestancias(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
 cuenta bigint;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT count(codigo)  FROM 
           (SELECT t.codigo FROM todasestancias t, actividades a 
             WHERE substring(t.codigo from 1 for 6) = ' || quote_literal(upper(plantaedificio)) ||
             ' AND t.actividad = a.codactividad
               AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ' GROUP BY t.codigo) AS foo;' 
  INTO cuenta;
 RETURN cuenta;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_numestancias(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene el nº de estancias de un tipo de grupo de actividad (crue, u21,activresum) y una denominación de grupo de actividad en una planta de edificio.
SINTAXIS
SELECT quest_plantaedificio_numestancias(''crue'',''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numestancias(integer, character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
   uso int4;
   adscripcion varchar;
   plantaedificio varchar;
   cuenta int8;
	
BEGIN
uso := $1;
adscripcion := upper($2);
plantaedificio := upper($3);
-- Control de errores
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;


   SELECT count(DISTINCT codigo) INTO cuenta FROM 
   todasestancias
   WHERE actividad = uso
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 6) =  plantaedificio;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numestancias(integer, character varying, character varying) IS 'Obtiene el nº de estancias de un departamento SIGUANET y una actividad en un edificio y una planta de la Universidad.
SINTAXIS:
- SELECT * FROM quest_plantaedificio_numestancias(7,''B101'', ''0037PB'');
- SELECT quest_plantaedificio_numestancias(7,''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numestancias(character varying, character varying, character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    agrupa varchar;
    denoactividad varchar;
    adscripcion varchar;
    plantaedificio varchar;
    cuenta int8;
	
BEGIN
agrupa := lower($1);
adscripcion := upper($3);
plantaedificio := upper($4);

IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
   RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
END IF;

IF agrupa = 'crue' THEN
	denoactividad := upper($2);
        IF denoactividad NOT IN ( SELECT crue FROM actividades) THEN
	   RAISE EXCEPTION 'La actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
		SELECT count(codigo) INTO cuenta FROM 
		(SELECT t.codigo FROM todasestancias t, departamentossigua d, actividades a 
		WHERE t.coddpto = d.cod_dpto_sigua
		AND t.actividad = a.codactividad
		AND t.coddpto = adscripcion 
		AND a.crue = denoactividad
		AND substring(t.codigo from 1 for 6) = plantaedificio
		GROUP BY t.codigo) AS foo;
		RETURN cuenta;
ELSEIF agrupa = 'u21' THEN
	denoactividad := upper($2);
        IF denoactividad NOT IN ( SELECT u21 FROM actividades) THEN
	   RAISE EXCEPTION 'La actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
		SELECT count(codigo) INTO cuenta FROM 
		(SELECT t.codigo FROM todasestancias t, departamentossigua d, actividades a 
		WHERE t.coddpto = d.cod_dpto_sigua
		AND t.actividad = a.codactividad
		AND t.coddpto = adscripcion 
		AND a.u21 = denoactividad
		AND substring(t.codigo from 1 for 6) = plantaedificio
		GROUP BY t.codigo) AS foo;
		RETURN cuenta;
ELSEIF agrupa = 'activresum' THEN
	denoactividad := $2;
        IF denoactividad NOT IN ( SELECT activresum FROM actividades) THEN
	   RAISE EXCEPTION 'La actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
		SELECT count(codigo) INTO cuenta FROM 
		(SELECT t.codigo FROM todasestancias t, departamentossigua d, actividades a 
		WHERE t.coddpto = d.cod_dpto_sigua
		AND t.actividad = a.codactividad
		AND t.coddpto = adscripcion 
		AND a.activresum = denoactividad
		AND substring(t.codigo from 1 for 6) = plantaedificio
		GROUP BY t.codigo) AS foo;
		RETURN cuenta;
ELSE
     RAISE exception 'No existe un campo valido con ese nombre: (%). Debe de ser u21, activresum o crue ', $1;
END IF;

END;

$_$;

COMMENT ON FUNCTION quest_plantaedificio_numestancias(character varying, character varying, character varying, character varying) IS 'Obtiene el nº de estancias de un departamento SIGUANET y un grupo de actividad en un edificio y una planta de la Universidad.
SINTAXIS:
- SELECT * FROM quest_plantaedificio_numestancias(''crue'',''DOCENCIA'',''B101'',''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numestanciasdocentes(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(codigo) FROM 
	(SELECT t.codigo from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE upper(a.activresum) = 'DOCENCIA'
         AND substring(codigo from 1 for 6) = $1
         group by t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numestanciasdocentes(character varying) IS 'Obtiene el nº de estancias de una planta de un edificio de la Universidad cuya actividad pertenece al grupo "Docencia" de la tabla actividades. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_numestanciasdocentes(''0011PB'');
- select quest_plantaedificio_numestanciasdocentes(''0011PB'');';

CREATE FUNCTION quest_plantaedificio_numestanciasdocentes(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(codigo) FROM 
	(SELECT t.codigo from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE upper(a.activresum) = 'DOCENCIA'
	 AND t.coddpto = upper($1)
         AND substring(t.codigo from 1 for 6) = upper($2)
         group by t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numestanciasdocentes(character varying, character varying) IS 'Obtiene el nº de estancias adscritas a un departamento SIGUANET en una planta de un edificio de la Universidad cuya actividad pertenece al grupo "Docencia" de la tabla actividades. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_numestanciasdocentes(''B101'', ''0037PB'');
- select quest_plantaedificio_numestanciasdocentes(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numestanciasnoocupadas(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1
	AND substring(codigo from 1 for 6) = upper($2);
$_$;

CREATE FUNCTION quest_plantaedificio_numestanciasnoocupadas(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
 cuenta bigint;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT count(DISTINCT t.codigo) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo NOT IN (SELECT codigo FROM todaspersonas)
             AND substring(t.codigo from 1 for 6) = ' || quote_literal(upper(plantaedificio)) || ';' INTO cuenta;
 RETURN cuenta;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_numestanciasnoocupadas(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene el nº de estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio.
SINTAXIS
SELECT quest_plantaedificio_numestanciasnoocupadas(''crue'',''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numestanciasocupadas(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND substring(codigo from 1 for 6) = upper($1)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_plantaedificio_numestanciasocupadas(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND coddpto = upper($1)
	AND substring(codigo from 1 for 6) = upper($2)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_plantaedificio_numestanciasocupadas(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1
	AND substring(codigo from 1 for 6) = upper($2);
$_$;

CREATE FUNCTION quest_plantaedificio_numestanciasocupadas(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
 cuenta bigint;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT count(DISTINCT t.codigo) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo IN (SELECT codigo FROM todaspersonas)
             AND substring(t.codigo from 1 for 6) = ' || quote_literal(upper(plantaedificio)) || ';' INTO cuenta;
 RETURN cuenta;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_numestanciasocupadas(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene el nº de estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio.
SINTAXIS
SELECT quest_plantaedificio_numestanciasocupadas(''crue'',''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numestanciasutiles(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(*) FROM 
	(SELECT count(t.codigo) from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE a.util = true AND substring(codigo from 1 for 6) = upper($1) GROUP BY t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numestanciasutiles(character varying) IS 'Obtiene el nº de estancias útiles de la planta de un edificio de la Universidad. 
Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_numestanciasutiles(''0037PB'');
- select quest_plantaedificio_numestanciasutiles(''0037P1'');
';

CREATE FUNCTION quest_plantaedificio_numestanciasutiles(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(*) FROM 
	(SELECT count(t.codigo)
	 FROM todasestancias t 
	 JOIN actividades a ON t.actividad = a.codactividad 
         WHERE a.util = true 
	 AND t.coddpto = upper($1)
	 AND substring(t.codigo from 1 for 6) = upper($2)
	 GROUP BY t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numestanciasutiles(character varying, character varying) IS 'Obtiene el nº de estancias útiles adscritas a un departamento SIGUANET en una planta de un edificio de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_numestanciasutiles(''B101'', ''0037PB'');
- select quest_plantaedificio_numestanciasutiles(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numexternos(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalexternos WHERE substring(codigo from 1 for 6) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numexternos(character varying) IS 'Obtiene el nº de externos en un edificio y una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantaedificio_numexternos(''0037PB'');
- select quest_plantaedificio_numexternos(''0037PB'');
';

CREATE FUNCTION quest_plantaedificio_numexternos(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalexternos
	WHERE cod_dpto_sigua = upper($1)
	AND substring(codigo from 1 for 6) = upper($2)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numexternos(character varying, character varying) IS 'Obtiene el total de externos de un departamento SIGUANET en una planta de un edificio de la Universidad. 
Se ejecuta de dos formas:
- SELECT * FROM quest_plantaedificio_numexternos(''B101'', ''0037PB'');
- SELECT quest_plantaedificio_numexternos(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numexternos(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM personalexternos WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 6) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_plantaedificio_numexternos(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM personalexternos WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 6) = ' || quote_literal(upper(plantaedificio)) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_numexternos(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene el nº de empleados EXTERNOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio.
SINTAXIS:
- SELECT quest_plantaedificio_numexternos(''crue'', ''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numpas(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalpas WHERE substring(codigo from 1 for 6) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numpas(character varying) IS 'Obtiene el nº de PAS en un edificio y una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantaedificio_numpas(''0037PB'');
- select quest_plantaedificio_numpas(''0037PB'');
';

CREATE FUNCTION quest_plantaedificio_numpas(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalpas
	WHERE cod_unidad = upper($1)
	AND substring(codigo from 1 for 6) = upper($2)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numpas(character varying, character varying) IS 'Obtiene el total de PAS de un departamento SIGUANET en una planta de un edificio de la Universidad. 
Se ejecuta de dos formas:
- SELECT * FROM quest_plantaedificio_numpas(''B101'', ''0037PB'');
- SELECT quest_plantaedificio_numpas(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numpas(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM personalpas WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 6) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_plantaedificio_numpas(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM personalpas WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 6) = ' || quote_literal(upper(plantaedificio)) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_numpas(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene el nº de PAS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio.
SINTAXIS:
- SELECT quest_plantaedificio_numpas(''crue'', ''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numpdi(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalpdi WHERE substring(codigo from 1 for 6) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numpdi(character varying) IS 'Obtiene el nº de PDI en un edificio y una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantaedificio_numpdi(''0037PB'');
- select quest_plantaedificio_numpdi(''0037PB'');
';

CREATE FUNCTION quest_plantaedificio_numpdi(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalpdi
	WHERE cod_depto = upper($1)
	AND substring(codigo from 1 for 6) = upper($2)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numpdi(character varying, character varying) IS 'Obtiene el total de PDI de un departamento SIGUANET en una planta de un edificio de la Universidad. 
Se ejecuta de dos formas:
- SELECT * FROM quest_plantaedificio_numpdi(''B101'', ''0037PB'');
- SELECT quest_plantaedificio_numpdi(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numpdi(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM personalpdi WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 6) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_plantaedificio_numpdi(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM personalpdi WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 6) = ' || quote_literal(upper(plantaedificio)) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_numpdi(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene el nº de PDI ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio.
SINTAXIS:
- SELECT quest_plantaedificio_numpdi(''crue'', ''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numpdicargos(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalpdi_cargos WHERE substring(codigo from 1 for 6) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numpdicargos(character varying) IS 'Obtiene el nº de PDI que desempeñan cargo en un edificio y una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantaedificio_numpdicargos(''0037PB'');
- select quest_plantaedificio_numpdicargos(''0037PB'');
';

CREATE FUNCTION quest_plantaedificio_numpdicargos(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalpdi_cargos p
	JOIN cargos_dpto cd ON p.cod_cargo = cd.cod_cargo
	WHERE cd.coddpto = upper($1)
	AND substring(p.codigo from 1 for 6) = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numpdicargos(character varying, character varying) IS 'Obtiene el nº de PDI que desempeñan cargo en un departamento sigua y en una planta de un edificio de la Universidad. 
Se ejecuta de dos formas:
- SELECT * FROM quest_plantaedificio_numpdicargos(''B101'', ''0037PB'');
- SELECT quest_plantaedificio_numpdicargos(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numpersonas(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(DISTINCT nif) FROM todaspersonas
   WHERE substring (codigo from 1 for 6) = upper($1) AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numpersonas(character varying) IS 'Obtiene el nº de personas en un edificio y una planta de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_numpersonas(''0037PB'');
- select quest_plantaedificio_numpersonas(''0037PB'');
';

CREATE FUNCTION quest_plantaedificio_numpersonas(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$DECLARE
    cuenta int8;
    adscripcion varchar;
    plantaedificio varchar;    
BEGIN
   adscripcion := upper($1);
   plantaedificio := upper($2);
   IF adscripcion NOT IN (SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT nif) INTO cuenta 
   FROM todaspersonas
   WHERE cod_depto = adscripcion
   AND substring(codigo from 1 for 6) = plantaedificio
   AND codigo != '0000PB997';

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_numpersonas(character varying, character varying) IS 'Obtiene el nº de personas de un departamento sigua en una planta de un edificio de la Universidad, indicando si es pas, pdi, becario, externo o alguna de sus combinaciones
SINTAXIS:
- SELECT quest_plantaedificio_numpersonas(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_numpersonas(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM todaspersonas WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 6) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_plantaedificio_numpersonas(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM todaspersonas WHERE codigo IN
            (SELECT codigo FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
            ' AND substring(codigo FROM 1 FOR 6) = ' || quote_literal(upper(plantaedificio)) || ');' INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_numpersonas(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene el nº de personas ubicadas en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad, en una planta de edificio.
SINTAXIS:
- SELECT quest_plantaedificio_numpersonas(''crue'', ''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_obteneradmonnoocupados(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias
WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
AND actividad = 8
AND codplantaedif = $1;
$_$;

CREATE FUNCTION quest_plantaedificio_obteneradmonnoocupados(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND coddpto = upper($1)
	AND codplantaedif = upper($2);
$_$;

CREATE FUNCTION quest_plantaedificio_obteneradmonocupados(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias
WHERE codigo IN (SELECT codigo FROM todaspersonas)
AND actividad = 8
AND codplantaedif = $1;
$_$;

CREATE FUNCTION quest_plantaedificio_obteneradmonocupados(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND coddpto = upper($1)
	AND codplantaedif = upper($2);
$_$;

CREATE FUNCTION quest_plantaedificio_obtenerdepartamentossigua(character varying) RETURNS SETOF record
    LANGUAGE plpgsql
    AS $_$
DECLARE
   micursor refcursor;
   plantaedificio varchar;
   fila record;

BEGIN
   plantaedificio := upper($1);

-- apertura del cursor
  OPEN micursor FOR

SELECT 
	te.coddpto AS cod, 
	d.txt_dpto_sigua AS txt, 
	count(te.codigo) as conteo,
	SUM(st_area(te.geometria)) As superficie
FROM 
	todasestancias te, 
	departamentossigua d
WHERE 
	te.coddpto = d.cod_dpto_sigua
	 and te.codigo LIKE plantaedificio || '%'
	 and te.actividad <> 93
	 and te.coddpto <> '07.018'
GROUP BY te.coddpto, d.txt_dpto_sigua
ORDER BY superficie desc;
 
 

  FETCH micursor INTO fila;
  WHILE FOUND LOOP
    RETURN NEXT fila;
    FETCH micursor INTO fila;
  END LOOP;
  RETURN;

END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_obtenerdepartamentossigua(character varying) IS 'Obtiene un listado de departamentossigua de una planta de edificio y su nº de estancias.
SINTAXIS:
SELECT * FROM quest_plantaedificio_obtenerdepartamentossigua(''0028P1'') AS (cod varchar, txt varchar, conteo int8, superficie float8);
';

CREATE FUNCTION quest_plantaedificio_obtenerdespachosnoocupados(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias
WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
AND actividad = 7
AND codplantaedif = $1;
$_$;

CREATE FUNCTION quest_plantaedificio_obtenerdespachosnoocupados(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND coddpto = upper($1)
	AND codplantaedif = upper($2);
$_$;

CREATE FUNCTION quest_plantaedificio_obtenerdespachosocupados(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias
WHERE codigo IN (SELECT codigo FROM todaspersonas)
AND actividad = 7
AND codplantaedif = $1;
$_$;

CREATE FUNCTION quest_plantaedificio_obtenerdespachosocupados(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND coddpto = upper($1)
	AND codplantaedif = upper($2);
$_$;

CREATE FUNCTION quest_plantaedificio_obtenerestancias(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila quest_estancias%ROWTYPE;
    adscripcion varchar;
    plantaedificio varchar;    
BEGIN
   adscripcion := upper($1);
   plantaedificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   FOR fila IN 
	SELECT * FROM quest_estancias 
	WHERE coddpto = adscripcion
	AND codplantaedif = plantaedificio
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_obtenerestancias(character varying, character varying) IS 'Obtiene todas las estancias de un departamento sigua en una planta de un edificio de la Universidad
Ejemplo:
- select * from quest_plantaedificio_obtenerestancias(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_obtenerestancias(integer, character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila quest_estancias%ROWTYPE;
    act integer;
    plantaedificio varchar;    
BEGIN
   act := $1;
   plantaedificio := upper($2);
   IF act NOT IN (SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION '% no es una actividad definida en la tabla actividades.', act;
   END IF;

   FOR fila IN 
	SELECT * FROM quest_estancias 
	WHERE actividad = act
	AND codplantaedif = plantaedificio
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_obtenerestancias(integer, character varying) IS 'Obtiene todas las estancias designadas para un uso SIGUANET en una planta de un edificio de la Universidad
Ejemplo:
- select * from quest_plantaedificio_obtenerestancias(7, ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_obtenerestancias(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $$
DECLARE
  validacion integer;
  c refcursor;
  estancia quest_estancias%ROWTYPE;
  tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
        WHEN 'activresum' THEN 'denogrupo'
        WHEN 'crue' THEN 'denocrue'
        WHEN 'u21' THEN 'denou21'
        END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_estancias WHERE substring(codigo from 1 for 6) = ' || quote_literal(upper(plantaedificio)) || ' AND lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || ';';
  LOOP
   FETCH c INTO estancia;
   EXIT WHEN NOT FOUND;
   RETURN NEXT estancia;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_obtenerestancias(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene las estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio.
SINTAXIS
SELECT quest_ua_obtenerestancias(''crue'',''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_obtenerestanciasdocentes(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias 
	WHERE upper(denogrupo) = 'DOCENCIA'
	AND codplantaedif = $1;
$_$;

CREATE FUNCTION quest_plantaedificio_obtenerestanciasdocentes(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias 
	WHERE upper(denogrupo) = 'DOCENCIA'
	AND coddpto = upper($1)
	AND codplantaedif = upper($2);
$_$;

CREATE FUNCTION quest_plantaedificio_obtenerestanciasnoocupadas(integer, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1
	AND codplantaedif = upper($2);
$_$;

CREATE FUNCTION quest_plantaedificio_obtenerestanciasnoocupadas(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $$
DECLARE
 validacion integer;
 c refcursor;
 estancia quest_estancias%ROWTYPE;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 SELECT INTO tipo_ CASE lower(tipo)
  WHEN 'activresum' THEN 'denogrupo'
  WHEN 'crue' THEN 'denocrue'
  WHEN 'u21' THEN 'denou21'
 END;
 OPEN c FOR EXECUTE 'SELECT * FROM quest_estancias 
                      WHERE lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || 
                    ' AND codigo NOT IN (SELECT codigo FROM todaspersonas) 
                      AND substring(codigo from 1 for 6) = ' || quote_literal(upper(plantaedificio)) || ';' ;
 LOOP
  FETCH c INTO estancia;
  EXIT WHEN NOT FOUND;
  RETURN NEXT estancia;
 END LOOP;
 RETURN;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_obtenerestanciasnoocupadas(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene las estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio.
SINTAXIS
SELECT * FROM quest_plantaedificio_obtenerestanciasnoocupadas(''crue'',''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_obtenerestanciasocupadas(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias 
	WHERE (actividad <= 50 OR actividad = 99)
	AND codigo IN (SELECT codigo FROM todaspersonas)
	AND codplantaedif = $1
	AND codigo != '0000PB997';

$_$;

CREATE FUNCTION quest_plantaedificio_obtenerestanciasocupadas(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias 
	WHERE (actividad <= 50 OR actividad = 99)
	AND codigo IN (SELECT codigo FROM todaspersonas)
	AND coddpto = upper($1)
	AND codplantaedif = upper($2)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_plantaedificio_obtenerestanciasocupadas(integer, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1
	AND codplantaedif = upper($2);
$_$;

CREATE FUNCTION quest_plantaedificio_obtenerestanciasocupadas(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $$
DECLARE
 validacion integer;
 c refcursor;
 estancia quest_estancias%ROWTYPE;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 SELECT INTO tipo_ CASE lower(tipo)
  WHEN 'activresum' THEN 'denogrupo'
  WHEN 'crue' THEN 'denocrue'
  WHEN 'u21' THEN 'denou21'
 END;
 OPEN c FOR EXECUTE 'SELECT * FROM quest_estancias 
                      WHERE lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || 
                    ' AND codigo IN (SELECT codigo FROM todaspersonas) 
                      AND substring(codigo from 1 for 6) = ' || quote_literal(upper(plantaedificio)) || ';' ;
 LOOP
  FETCH c INTO estancia;
  EXIT WHEN NOT FOUND;
  RETURN NEXT estancia;
 END LOOP;
 RETURN;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_obtenerestanciasocupadas(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene las estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio.
SINTAXIS
SELECT * FROM quest_plantaedificio_obtenerestanciasocupadas(''crue'',''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_obtenerestanciasutiles(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila quest_estancias%ROWTYPE;
    adscripcion varchar;
    plantaedificio varchar;
    
BEGIN
   adscripcion := upper($1);
   plantaedificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   FOR fila IN 
	SELECT * FROM quest_estancias t JOIN actividades a ON t.actividad = a.codactividad 
	WHERE a.util = true 
	AND t.coddpto = adscripcion
	AND t.codplantaedif = plantaedificio
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_obtenerestanciasutiles(character varying, character varying) IS 'Obtiene todas las estancias útiles de un departamento sigua en una planta de un edificio de la Universidad
Ejemplo:
- select * from quest_plantaedificio_obtenerestanciasutiles(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficie(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
SELECT sum(st_area(geometria)) FROM todasestancias
WHERE substring(codigo from 1 for 6) = upper($1);
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficie(character varying) IS 'Obtiene la superficie total de un edificio y una planta de la Universidad.';

CREATE FUNCTION quest_plantaedificio_superficie(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria)) 
	FROM todasestancias
	WHERE coddpto = upper($1)
	AND substring(codigo from 1 for 6) = upper($2);
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficie(character varying, character varying) IS 'Obtiene la superficie total de las estancias de un departamento SIGUANET en una planta de un edificio de la Universidad.';

CREATE FUNCTION quest_plantaedificio_superficie(integer, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria)) 
	FROM todasestancias
	WHERE actividad = $1
	AND substring(codigo from 1 for 6) = upper($2);
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficie(integer, character varying) IS 'Obtiene la superficie total de las estancias designadas para un uso SIGUANET en una planta de un edificio de la Universidad.';

CREATE FUNCTION quest_plantaedificio_superficie(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
 superficie double precision;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT sum(st_area(geometria)) FROM 
           (SELECT t.geometria FROM todasestancias t, actividades a 
             WHERE substring(t.codigo from 1 for 6) = ' || quote_literal(upper(plantaedificio)) ||
             ' AND t.actividad = a.codactividad
               AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ') AS foo;'
  INTO superficie;
 RETURN superficie;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_superficie(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene la superficie que ocupan las estancias de un tipo de grupo de actividad (crue, u21,activresum) y una denominación de grupo de actividad en una planta de edificio.
SINTAXIS
SELECT quest_plantaedificio_superficie(''crue'',''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieadmonnoocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    plantaedificio varchar;
BEGIN
   plantaedificio := upper($1);
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND substring(codigo from 1 for 6) = plantaedificio;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_plantaedificio_superficieadmonnoocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    plantaedificio varchar;    
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   plantaedificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 6) = plantaedificio;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieadmonnoocupados(character varying, character varying) IS 'Obtiene la superficie de administraciones no ocupadas de un departamento SIGUANET en una planta de un edificio de la Universidad. 
SINTAXIS:
- select * from quest_plantaedificio_superficieadmonnoocupados(''B101'', ''0037PB'');
- select quest_plantaedificio_superficieadmonnoocupados(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieadmonocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    plantaedificio varchar;
BEGIN
   plantaedificio := upper($1);
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND substring(codigo from 1 for 6) = plantaedificio;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_plantaedificio_superficieadmonocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    plantaedificio varchar;    
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   plantaedificio := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 6) = plantaedificio;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieadmonocupados(character varying, character varying) IS 'Obtiene la superficie de administraciones ocupadas de un departamento SIGUANET en una planta de un edificio de la Universidad. 
SINTAXIS:
- select * from quest_plantaedificio_superficieadmonocupados(''B101'', ''0037PB'');
- select quest_plantaedificio_superficieadmonocupados(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficiedespachosnoocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    plantaedificio varchar;
BEGIN
   plantaedificio := upper($1);
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND substring(codigo from 1 for 6) = plantaedificio;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_plantaedificio_superficiedespachosnoocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    plantaedificio varchar;    
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   plantaedificio := upper($2);   
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 6) = plantaedificio;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficiedespachosnoocupados(character varying, character varying) IS 'Obtiene la superficie de despachos no ocupados de un departamento SIGUANET en una planta de un edificio de la Universidad. 
SINTAXIS:
- select * from quest_plantaedificio_superficiedespachosnoocupados(''B101'', ''0037PB'');
- select quest_plantaedificio_superficiedespachosnoocupados(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficiedespachosocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    plantaedificio varchar;
BEGIN
   plantaedificio := upper($1);
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND substring(codigo from 1 for 6) = plantaedificio;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_plantaedificio_superficiedespachosocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    plantaedificio varchar;    
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   plantaedificio := upper($2);   
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 6) = plantaedificio;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficiedespachosocupados(character varying, character varying) IS 'Obtiene la superficie de despachos ocupados de un departamento SIGUANET en una planta de un edificio de la Universidad. 
SINTAXIS:
- select * from quest_plantaedificio_superficiedespachosocupados(''B101'', ''0037PB'');
- select quest_plantaedificio_superficiedespachosocupados(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficiedocente(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE upper(a.activresum) = 'DOCENCIA'
        AND substring(codigo from 1 for 6) = $1;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficiedocente(character varying) IS 'Obtiene la superficie total de las estancias docentes de una planta de un edificio de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_superficiedocente(''0011PB'');
- select quest_plantaedificio_superficiedocente(''0011PB'');';

CREATE FUNCTION quest_plantaedificio_superficiedocente(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE upper(a.activresum) = 'DOCENCIA'
	AND t.coddpto = upper($1)
        AND substring(t.codigo from 1 for 6) = upper($2);
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficiedocente(character varying, character varying) IS 'Obtiene la superficie total de las estancias docentes de una departamento SIGUANET en una planta de un edificio de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_superficiedocente(''B101'', ''0037PB'');
- select quest_plantaedificio_superficiedocente(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieestanciasnoocupadas(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4;
  plantaedificio varchar;
  superficie float8;
BEGIN
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;

        uso := $1;
        plantaedificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = uso
        AND substring(codigo from 1 for 6) = plantaedificio;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieestanciasnoocupadas(integer, character varying) IS 'Obtiene la superficie de las estancias no ocupadas de una determinada actividad SIGUANET en un edificio y una planta de la Universidad. Se ejecuta de dos formas:
- select * from quest_plantaedificio_superficieestanciasnoocupadas(50, ''0037PB'');
- select quest_plantaedificio_superficieestanciasnoocupadas(50, ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieestanciasnoocupadas(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
 superficie double precision;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT sum(st_area(t.geometria)) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo NOT IN (SELECT codigo FROM todaspersonas) 
             AND substring(t.codigo from 1 for 6) = ' || quote_literal(upper(plantaedificio)) || ';' INTO superficie;
 RETURN superficie;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_superficieestanciasnoocupadas(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene la superficie de las estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio.
SINTAXIS
SELECT quest_plantaedificio_superficieestanciasnoocupadas(''crue'',''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieestanciasocupadas(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND substring(codigo from 1 for 6) = upper($1)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_plantaedificio_superficieestanciasocupadas(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4;
  plantaedificio varchar;
  superficie float8;
BEGIN
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;

        uso := $1;
        plantaedificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = uso
        AND substring(codigo from 1 for 6) = plantaedificio;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieestanciasocupadas(integer, character varying) IS 'Obtiene la superficie de las estancias ocupadas de una determinada actividad SIGUANET en un edificio y una planta de la Universidad. Se ejecuta de dos formas:
- select * from quest_plantaedificio_superficieestanciasocupadas(50, ''0037PB'');
- select quest_plantaedificio_superficieestanciasocupadas(50, ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieestanciasocupadas(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4[];
  plantaedificio varchar;
  superficie float8;
BEGIN
        uso := $1;
        plantaedificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = ANY (uso)
        AND substring(codigo from 1 for 6) = plantaedificio;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieestanciasocupadas(integer[], character varying) IS 'Obtiene la superficie de las estancias ocupadas de una lista de actividades SIGUANET en un edificio y una planta de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_superficieestanciasocupadas(ARRAY[1,2,3,5], ''0037PB'');
- select quest_plantaedificio_superficieestanciasocupadas(ARRAY[1,2,3,5], ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieestanciasocupadas(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND coddpto = upper($1)
	AND substring(codigo from 1 for 6) = upper($2)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_plantaedificio_superficieestanciasocupadas(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4[];
  plantaedificio varchar;  
  adscripcion varchar;
  superficie float8;
BEGIN
        uso := $1;
        adscripcion := upper($2);
        plantaedificio := upper($3);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = ANY (uso)
	AND coddpto = adscripcion
	AND substring(codigo from 1 for 6) = plantaedificio
	AND codigo != '0000PB997';

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieestanciasocupadas(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias ocupadas y adscritas a un departamento SIGUANET de una lista de actividades SIGUANET en una planta de un edificio de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_superficieestanciasocupadas(ARRAY[1,2,3,5], ''B101'', ''0037PB'');
- select quest_plantaedificio_superficieestanciasocupadas(ARRAY[1,2,3,5], ''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieestanciasocupadas(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
 superficie double precision;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT sum(st_area(t.geometria)) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo IN (SELECT codigo FROM todaspersonas) 
             AND substring(t.codigo from 1 for 6) = ' || quote_literal(upper(plantaedificio)) || ';' INTO superficie;
 RETURN superficie;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_superficieestanciasocupadas(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Obtiene la superficie de las estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio.
SINTAXIS
SELECT quest_plantaedificio_superficieestanciasocupadas(''crue'',''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieestanciasocupadasbec(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	plantaedificio varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        plantaedificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM becarios)
	AND actividad = $1
	AND substring(codigo from 1 for 6) = plantaedificio;
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieestanciasocupadasbec(integer, character varying) IS 'Obtiene la superficie de las estancias de un edificio y una planta ocupadas por becarios de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantaedificio_superficieestanciasocupadasbecarios(8, ''0037PB'');
- SELECT quest_plantaedificio_superficieestanciasocupadasbecarios(8, ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieestanciasocupadasbec(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	plantaedificio varchar;
	superficie float8;
BEGIN
        uso := $1;
        plantaedificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM becarios)
	AND actividad = ANY ($1)
	AND substring(codigo from 1 for 6) = plantaedificio;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieestanciasocupadasbec(integer[], character varying) IS 'Obtiene la superficie de las estancias de un edificio y una planta ocupadas por becarios de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantaedificio_superficieestanciasocupadasbecarios(ARRAY[8,9,16],''0037PB'');
- SELECT quest_plantaedificio_superficieestanciasocupadasbecarios(ARRAY[8,9,16],''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieestanciasocupadasbec(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM becarios)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 1 for 6) = upper($3)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieestanciasocupadasbec(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por becarios de una lista de actividades SIGUANET y en una planta de un edificio de la Universidad. 
SINTAXIS:
- select * from quest_plantaedificio_superficieestanciasocupadasbec(ARRAY[1,2,3,5], ''B101'', ''0037PB'');
- select quest_plantaedificio_superficieestanciasocupadasbec(ARRAY[1,2,3,5], ''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieestanciasocupadasext(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	plantaedificio varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        plantaedificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalexternos)
	AND actividad = $1
	AND substring(codigo from 1 for 6) = plantaedificio;
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieestanciasocupadasext(integer, character varying) IS 'Obtiene la superficie de las estancias de un edificio y una planta ocupadas por externos de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantaedificio_superficieestanciasocupadasext(8, ''0037PB'');
- SELECT quest_plantaedificio_superficieestanciasocupadasext(8, ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieestanciasocupadasext(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	plantaedificio varchar;
	superficie float8;
BEGIN
        uso := $1;
        plantaedificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalexternos)
	AND actividad = ANY ($1)
	AND substring(codigo from 1 for 6) = plantaedificio;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieestanciasocupadasext(integer[], character varying) IS 'Obtiene la superficie de las estancias de un edificio y una planta ocupadas por externos de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantaedificio_superficieestanciasocupadasext(ARRAY[8,9,16],''0037PB'');
- SELECT quest_plantaedificio_superficieestanciasocupadasext(ARRAY[8,9,16],''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieestanciasocupadasext(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalexternos)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 1 for 6) = upper($3)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieestanciasocupadasext(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por externos de una lista de actividades SIGUANET y en una planta de un edificio de la Universidad. 
SINTAXIS:
- select * from quest_plantaedificio_superficieestanciasocupadasext(ARRAY[1,2,3,5], ''B101'', ''0037PB'');
- select quest_plantaedificio_superficieestanciasocupadasext(ARRAY[1,2,3,5], ''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieestanciasocupadaspas(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	plantaedificio varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        plantaedificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpas)
	AND actividad = $1
	AND substring(codigo from 1 for 6) = plantaedificio;
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieestanciasocupadaspas(integer, character varying) IS 'Obtiene la superficie de las estancias de un edificio y una planta ocupadas por PAS de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantaedificio_superficieestanciasocupadaspas(8, ''0037PB'');
- SELECT quest_plantaedificio_superficieestanciasocupadaspas(8, ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieestanciasocupadaspas(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	plantaedificio varchar;
	superficie float8;
BEGIN
        uso := $1;
        plantaedificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpas)
	AND actividad = ANY ($1)
	AND substring(codigo from 1 for 6) = plantaedificio;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieestanciasocupadaspas(integer[], character varying) IS 'Obtiene la superficie de las estancias de un edificio y una planta ocupadas por PAS de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantaedificio_superficieestanciasocupadaspas(ARRAY[8,9,16],''0037PB'');
- SELECT quest_plantaedificio_superficieestanciasocupadaspas(ARRAY[8,9,16],''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieestanciasocupadaspas(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpas)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 1 for 6) = upper($3)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieestanciasocupadaspas(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por PAS de una lista de actividades SIGUANET y en una planta de un edificio de la Universidad. 
SINTAXIS:
- select * from quest_plantaedificio_superficieestanciasocupadaspas(ARRAY[1,2,3,5], ''B101'', ''0037PB'');
- select quest_plantaedificio_superficieestanciasocupadaspas(ARRAY[1,2,3,5], ''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieestanciasocupadaspdi(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	plantaedificio varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        plantaedificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpdi)
	AND actividad = $1
	AND substring(codigo from 1 for 6) = plantaedificio;
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieestanciasocupadaspdi(integer, character varying) IS 'Obtiene la superficie de las estancias de un edificio y una planta ocupadas por PDI de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantaedificio_superficieestanciasocupadaspdi(8, ''0037PB'');
- SELECT quest_plantaedificio_superficieestanciasocupadaspdi(8, ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieestanciasocupadaspdi(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	plantaedificio varchar;
	superficie float8;
BEGIN
        uso := $1;
        plantaedificio := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpdi)
	AND actividad = ANY ($1)
	AND substring(codigo from 1 for 6) = plantaedificio;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieestanciasocupadaspdi(integer[], character varying) IS 'Obtiene la superficie de las estancias de un edificio y una planta ocupadas por PDI de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantaedificio_superficieestanciasocupadaspdi(ARRAY[8,9,16],''0037PB'');
- SELECT quest_plantaedificio_superficieestanciasocupadaspdi(ARRAY[8,9,16],''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieestanciasocupadaspdi(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpdi)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 1 for 6) = upper($3)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieestanciasocupadaspdi(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por PDI de una lista de actividades SIGUANET y en una planta de un edificio de la Universidad. 
SINTAXIS:
- select * from quest_plantaedificio_superficieestanciasocupadaspdi(ARRAY[1,2,3,5], ''B101'', ''0037PB'');
- select quest_plantaedificio_superficieestanciasocupadaspdi(ARRAY[1,2,3,5], ''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_superficieutil(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE a.util = true AND substring(t.codigo from 1 for 6) = upper($1);
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieutil(character varying) IS 'Obtiene la superficie útil de una planta de un edificio de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_superficieutil(''0037PB'');
- select quest_plantaedificio_superficieutil(''0037P1'');
';

CREATE FUNCTION quest_plantaedificio_superficieutil(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) 
	FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE a.util = true 
	AND t.coddpto = upper($1)
	AND substring(t.codigo from 1 for 6) = upper($2);
$_$;

COMMENT ON FUNCTION quest_plantaedificio_superficieutil(character varying, character varying) IS 'Obtiene la superficie útil de un departamento SIGUANET en una planta de un edificio de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantaedificio_superficieutil(''B101'', ''0037PB'');
- select quest_plantaedificio_superficieutil(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_ubicaciones(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE codplantaedif = $1;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_ubicaciones(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de una planta de un edificio de la Universidad.
SINTAXIS
- SELECT * FROM quest_plantaedificio_ubicaciones(''0011PB'')';

CREATE FUNCTION quest_plantaedificio_ubicaciones(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT DISTINCT v.* FROM quest_ubicaciones v
	JOIN quest_personas2 p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE p.cod_depto = upper($1)
	AND v.codplantaedif = upper($2)	
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_ubicaciones(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de las personas adscritas a un departamento SIGUANET en una planta de un edificio de la Universidad.
SINTAXIS
- select * from quest_plantaedificio_ubicaciones(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_ubicaciones(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones WHERE actividad = $1 AND substring(codigo from 1 for 6) = $2;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_ubicaciones(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en uan planta de un edificio.
SINTAXIS
- SELECT * FROM quest_plantaedificio_ubicaciones(7, ''0037PB'')';

CREATE FUNCTION quest_plantaedificio_ubicaciones(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND substring(codigo FROM 1 FOR 6) = ' || quote_literal(upper(plantaedificio)) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_ubicaciones(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para las personas ubicadas en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad, en una planta de edificio.
SINTAXIS:
- SELECT * FROM quest_plantaedificio_ubicaciones(''crue'', ''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_ubicacionesbecarios(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones
   WHERE esbecario = true
   AND locbecario = true
   AND reftbl = 'becarios' 
   AND (actividad <= 50 OR actividad = 99)
   AND codplantaedif = $1;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_ubicacionesbecarios(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de una planta de un edificio de la Universidad.
La condición where matiza que sólo devuelve estancias con BECARIOS
SINTAXIS
- SELECT * FROM quest_plantaedificio_ubicacionesbecarios(''0011PB'')';

CREATE FUNCTION quest_plantaedificio_ubicacionesbecarios(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_becarios p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.esbecario = true 
	AND v.locbecario = true
	AND v.reftbl = 'becarios' 
	AND p.cod_depto_centro_subunidad = upper($1)
	AND v.codplantaedif = upper($2)	
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_ubicacionesbecarios(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociación becario/estancia de los becarios adscritos a un departamento SIGUANET ubicados en una planta de un edificio de la Universidad.
SINTAXIS
- select * from quest_plantaedificio_ubicacionesbecarios(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_ubicacionesbecarios(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 AND substring(codigo from 1 for 6) = $2 
   AND esbecario = true 
   AND locbecario = true
   AND reftbl = 'becarios';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_ubicacionesbecarios(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion becario/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en una planta de un edificio.
SINTAXIS
- SELECT * FROM quest_plantaedificio_ubicacionesbecarios(7, ''0018PB'')';

CREATE FUNCTION quest_plantaedificio_ubicacionesbecarios(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND esbecario = true 
                       AND locbecario = true
                       AND reftbl = ''becarios''
                       AND substring(codigo FROM 1 FOR 6) = ' || quote_literal(upper(plantaedificio)) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_ubicacionesbecarios(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los BECARIOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio.
SINTAXIS:
- SELECT * FROM quest_plantaedificio_ubicacionesbecarios(''crue'', ''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_ubicacionescargos(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE espdicargo = true 
   AND locpdicargo = true
   AND reftbl ='personalpdi_cargos'
   AND (actividad <= 50 OR actividad = 99)
   AND codplantaedif = $1;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_ubicacionescargos(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de una planta de un edificio de la Universidad.
La condición where matiza que sólo devuelve estancias con CARGOS
SINTAXIS
- SELECT * FROM quest_plantaedificio_ubicacionescargos(''0011PB'')';

CREATE FUNCTION quest_plantaedificio_ubicacionescargos(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalpdi_cargos p ON v.nif = p.nif AND v.codigo = p.codigo
	JOIN cargos_dpto cd ON p.cod_cargo = cd.cod_cargo
	WHERE v.espdicargo = true 
	AND v.locpdicargo = true
	AND v.reftbl ='personalpdi_cargos'
	AND cd.coddpto = upper($1)
	AND v.codplantaedif = upper($2)	
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_ubicacionescargos(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociación pdicargo/estancia de los cargos adscritos a un departamento SIGUANET ubicados en una planta de un edificio de la Universidad.
SINTAXIS
- select * from quest_plantaedificio_ubicacionescargos(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_ubicacionesexternos(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones
   WHERE esexterno = true
   AND locexterno = true
   AND reftbl = 'personalexternos'   
   AND (actividad <= 50 OR actividad = 99)
   AND codplantaedif = $1;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_ubicacionesexternos(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de una planta de un edificio de la Universidad.
La condición where matiza que sólo devuelve estancias DE EXTERNOS
SINTAXIS
- SELECT * FROM quest_plantaedificio_ubicacionesexternos(''0011PB'')';

CREATE FUNCTION quest_plantaedificio_ubicacionesexternos(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalexternos p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.esexterno = true 
	AND v.locexterno = true
	AND v.reftbl = 'personalexternos'
	AND p.cod_dpto_sigua = upper($1)
	AND v.codplantaedif = upper($2)	
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_ubicacionesexternos(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion externo/estancia de los externos adscritos a un departamento SIGUANET ubicados en una planta de un edificio de la Universidad.
SINTAXIS
- select * from quest_plantaedificio_ubicacionesexternos(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_ubicacionesexternos(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 AND substring(codigo from 1 for 6) = $2 
   AND esexterno = true 
   AND locexterno = true
   AND reftbl = 'personalexternos';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_ubicacionesexternos(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion externo/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en una planta de un edificio.
SINTAXIS
- SELECT * FROM quest_plantaedificio_ubicacionesexternos(7, ''0018PB'')';

CREATE FUNCTION quest_plantaedificio_ubicacionesexternos(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND esexterno = true 
                       AND locexterno = true
                       AND reftbl = ''personalexternos''
                       AND substring(codigo FROM 1 FOR 6) = ' || quote_literal(upper(plantaedificio)) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_ubicacionesexternos(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los empleados EXTERNOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio.
SINTAXIS:
- SELECT * FROM quest_plantaedificio_ubicacionesexternos(''crue'', ''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_ubicacionespas(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_ubicaciones 
	WHERE espas = true
	AND locpas = true
	AND reftbl = 'personalpas'
	AND (actividad <= 50 OR actividad = 99)
	AND codplantaedif = $1;
$_$;

CREATE FUNCTION quest_plantaedificio_ubicacionespas(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalpas p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.espas = true 
	AND v.locpas = true
	AND v.reftbl = 'personalpas'
	AND p.cod_unidad = upper($1)
	AND v.codplantaedif = upper($2)	
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_ubicacionespas(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion pas/estancia de los pas adscritos a un departamento SIGUANET ubicados en una planta de un edificio de la Universidad.
La condición where matiza que sólo devuelve estancias con pas
SINTAXIS
- select * from quest_plantaedificio_ubicacionespas(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_ubicacionespas(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 AND substring(codigo from 1 for 6) = $2 
   AND espas = true 
   AND locpas = true
   AND reftbl = 'personalpas';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_ubicacionespas(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion PAS/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en una planta de un edificio.
SINTAXIS
- SELECT * FROM quest_plantaedificio_ubicacionespas(7, ''0018PB'')';

CREATE FUNCTION quest_plantaedificio_ubicacionespas(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND espas = true 
                       AND locpas = true
                       AND reftbl = ''personalpas''
                       AND substring(codigo FROM 1 FOR 6) = ' || quote_literal(upper(plantaedificio)) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_ubicacionespas(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los PAS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio.
SINTAXIS:
- SELECT * FROM quest_plantaedificio_ubicacionespas(''crue'', ''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_ubicacionespdi(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE espdi = true
   AND locpdi = true
   AND reftbl = 'personalpdi'
   AND (actividad <= 50 OR actividad = 99)
   AND codplantaedif = $1;
$_$;

COMMENT ON FUNCTION quest_plantaedificio_ubicacionespdi(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de una planta de un edificio de la Universidad.
La condición where matiza que sólo devuelve estancias con PDI
SINTAXIS
- SELECT * FROM quest_plantaedificio_ubicacionespdi(''0011PB'')';

CREATE FUNCTION quest_plantaedificio_ubicacionespdi(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalpdi p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.espdi = true 
	AND v.locpdi = true
	AND v.reftbl = 'personalpdi'
	AND p.cod_depto = upper($1)
	AND v.codplantaedif = upper($2)	
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_ubicacionespdi(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociación pdi/estancia de los pdi adscritos a un departamento SIGUANET ubicados en una planta de un edificio de la Universidad.
SINTAXIS
- select * from quest_plantaedificio_ubicacionespdi(''B101'', ''0037PB'');';

CREATE FUNCTION quest_plantaedificio_ubicacionespdi(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 AND substring(codigo from 1 for 6) = $2 
   AND espdi = true 
   AND locpdi = true
   AND reftbl = 'personalpdi';
$_$;

COMMENT ON FUNCTION quest_plantaedificio_ubicacionespdi(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion pdi/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en una planta de un edificio.
SINTAXIS
- SELECT * FROM quest_plantaedificio_ubicacionespdi(7, ''0018PB'')';

CREATE FUNCTION quest_plantaedificio_ubicacionespdi(tipo character varying, denominacion character varying, plantaedificio character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND espdi = true 
                       AND locpdi = true
                       AND reftbl = ''personalpdi''
                       AND substring(codigo FROM 1 FOR 6) = ' || quote_literal(upper(plantaedificio)) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantaedificio_ubicacionespdi(tipo character varying, denominacion character varying, plantaedificio character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los PDI ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de edificio.
SINTAXIS:
- SELECT * FROM quest_plantaedificio_ubicacionespdi(''crue'', ''DOCENCIA'', ''0037PB'');';

CREATE FUNCTION quest_plantabase_densidad(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantabase_superficieestanciasocupadas(ARRAY[4,5,7,8,9,16], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE actividad = ANY (ARRAY[4,5,7,8,9,16])
										       AND substring(codigo from 5 for 2) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_plantabase_densidad(character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantabase_densidad(''P1'');
- select quest_plantabase_densidad(''P1'');
';

CREATE FUNCTION quest_plantabase_densidad(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantabase_superficieestanciasocupadas(ARRAY[4,5,7,8,9,16], upper($1), upper($2)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE actividad = ANY (ARRAY[4,5,7,8,9,16])
						     AND coddpto = upper($1)
						     AND substring(codigo FROM 5 FOR 2) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_plantabase_densidad(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en un departamento SIGUANET y en una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantabase_densidad(''B101'', ''PB'');
- select quest_plantabase_densidad(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_densidad(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_plantabase_superficieestanciasocupadas($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM todaspersonas WHERE codigo IN (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 5 for 2) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_densidad(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en las estancias de una determinada actividad SIGUANET para toda una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantabase_densidad(4, ''PB'');
- select quest_plantabase_densidad(8, ''PB'');';

CREATE FUNCTION quest_plantabase_densidad(tipo character varying, denominacion character varying, planta character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  SELECT quest_plantabase_superficieestanciasocupadas(tipo, denominacion, planta) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM todaspersonas WHERE codigo IN
            (SELECT codigo FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
          '   AND substring(codigo FROM 5 FOR 2) = ' || quote_literal(upper(planta)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;   
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_densidad(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad, en una planta de la universidad. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_plantabase_densidad(''crue'', ''DOCENCIA'');';

CREATE FUNCTION quest_plantabase_densidadbecarios(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantabase_superficieestanciasocupadasbec(ARRAY[4,5,7], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM becarios)
										       AND actividad = ANY (ARRAY[4,5,7])
										       AND substring(codigo from 5 for 2) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_plantabase_densidadbecarios(character varying) IS 'Obtiene los m2 de espacio de trabajo por becarios en una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantabase_densidadbecarios(''P1'');
- select quest_plantabase_densidadbecarios(''P1'');
';

CREATE FUNCTION quest_plantabase_densidadbecarios(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantabase_superficieestanciasocupadasbec(ARRAY[4,5,7], $1, $2) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM becarios)
						     AND actividad = ANY (ARRAY[4,5,7])
						     AND coddpto = upper($1)
						     AND substring(codigo FROM 5 FOR 2) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_plantabase_densidadbecarios(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por becarios en un departamento SIGUANET y en una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantabase_densidadbecarios(''B101'', ''PB'');
- select quest_plantabase_densidadbecarios(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_densidadbecarios(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_plantabase_superficieestanciasocupadasbec($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM becarios WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM becarios)
                                           AND actividad = $1
                                           AND substring(codigo from 5 for 2) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_densidadbecarios(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por becario en las estancias de una determinada actividad SIGUANET para una planta de toda la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantabase_densidadbecarios(4, ''PB'');
- select quest_plantabase_densidadbecarios(8, ''PB'');';

CREATE FUNCTION quest_plantabase_densidadbecarios(tipo character varying, denominacion character varying, planta character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_plantabase_superficieestanciasocupadasbec(actlist, upper(planta)) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM becarios WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 5 FOR 2) = ' || quote_literal(upper(planta)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_densidadbecarios(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene los m2 de espacio de trabajo por BECARIO en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_plantabase_densidadbecarios(''crue'', ''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_densidadexternos(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantabase_superficieestanciasocupadasext(ARRAY[7,8], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalexternos)
										       AND actividad = ANY (ARRAY[7,8])
										       AND substring(codigo from 5 for 2) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_plantabase_densidadexternos(character varying) IS 'Obtiene los m2 de espacio de trabajo por externo en una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantabase_densidadexternos(''P1'');
- select quest_plantabase_densidadexternos(''P1'');
';

CREATE FUNCTION quest_plantabase_densidadexternos(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantabase_superficieestanciasocupadasext(ARRAY[7,8], $1, $2) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM personalexternos)
						     AND actividad = ANY (ARRAY[7,8])
						     AND coddpto = upper($1)
						     AND substring(codigo FROM 5 FOR 2) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_plantabase_densidadexternos(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por externos en un departamento SIGUANET y en una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantabase_densidadexternos(''B101'', ''PB'');
- select quest_plantabase_densidadexternos(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_densidadexternos(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_plantabase_superficieestanciasocupadasext($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM personalexternos WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalexternos)
                                           AND actividad = $1
                                           AND substring(codigo from 5 for 2) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_densidadexternos(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por externo en las estancias de una determinada actividad SIGUANET para una planta de toda la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantabase_densidadexternos(4, ''PB'');
- select quest_plantabase_densidadexternos(8, ''PB'');';

CREATE FUNCTION quest_plantabase_densidadexternos(tipo character varying, denominacion character varying, planta character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_plantabase_superficieestanciasocupadasext(actlist, upper(planta)) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM personalexternos WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 5 FOR 2) = ' || quote_literal(upper(planta)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_densidadexternos(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado EXTERNO en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_plantabase_densidadexternos(''crue'', ''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_densidadpas(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantabase_superficieestanciasocupadaspas(ARRAY[4,5,8,9,16], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpas)
										       AND actividad = ANY (ARRAY[4,5,8,9,16])
										       AND substring(codigo from 5 for 2) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_plantabase_densidadpas(character varying) IS 'Obtiene los m2 de espacio de trabajo por PAS en una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantabase_densidadpas(''P1'');
- select quest_plantabase_densidadpas(''P1'');
';

CREATE FUNCTION quest_plantabase_densidadpas(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantabase_superficieestanciasocupadaspas(ARRAY[4,5,8,9,16], $1, $2) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM personalpas)
						     AND actividad = ANY (ARRAY[4,5,8,9,16])
						     AND coddpto = upper($1)
						     AND substring(codigo FROM 5 FOR 2) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_plantabase_densidadpas(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por PAS en un departamento SIGUANET y en una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantabase_densidadpas(''B101'', ''PB'');
- select quest_plantabase_densidadpas(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_densidadpas(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_plantabase_superficieestanciasocupadaspas($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM personalpas WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpas)
                                           AND actividad = $1
                                           AND substring(codigo from 5 for 2) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_densidadpas(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado pas en las estancias de una determinada actividad SIGUANET para una planta de toda la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantabase_densidadpas(4, ''PB'');
- select quest_plantabase_densidadpas(8, ''PB'');';

CREATE FUNCTION quest_plantabase_densidadpas(tipo character varying, denominacion character varying, planta character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_plantabase_superficieestanciasocupadaspas(actlist, upper(planta)) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM personalpas WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 5 FOR 2) = ' || quote_literal(upper(planta)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_densidadpas(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado tipo PAS en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_plantabase_densidadpas(''crue'', ''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_densidadpdi(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantabase_superficieestanciasocupadaspdi(ARRAY[4,5,7], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpdi)
										       AND actividad = ANY (ARRAY[4,5,7])
										       AND substring(codigo from 5 for 2) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_plantabase_densidadpdi(character varying) IS 'Obtiene los m2 de espacio de trabajo por PDI en una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantabase_densidadpdi(''P1'');
- select quest_plantabase_densidadpdi(''P1'');
';

CREATE FUNCTION quest_plantabase_densidadpdi(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantabase_superficieestanciasocupadaspdi(ARRAY[4,5,7], $1, $2) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM personalpdi)
						     AND actividad = ANY (ARRAY[4,5,7])
						     AND coddpto = upper($1)
						     AND substring(codigo FROM 5 FOR 2) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_plantabase_densidadpdi(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por PDI en un departamento SIGUANET y en una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantabase_densidadpdi(''B101'', ''PB'');
- select quest_plantabase_densidadpdi(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_densidadpdi(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_plantabase_superficieestanciasocupadaspdi($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM personalpdi WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpdi)
                                           AND actividad = $1
                                           AND substring(codigo from 5 for 2) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_densidadpdi(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado pdi en las estancias de una determinada actividad SIGUANET para una planta de toda la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantabase_densidadpdi(4, ''PB'');
- select quest_plantabase_densidadpdi(8, ''PB'');';

CREATE FUNCTION quest_plantabase_densidadpdi(tipo character varying, denominacion character varying, planta character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_plantabase_superficieestanciasocupadaspdi(actlist, upper(planta)) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM personalpdi WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 5 FOR 2) = ' || quote_literal(upper(planta)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_densidadpdi(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado tipo PDI en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_plantabase_densidadpdi(''crue'', ''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_numadmonnoocupados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND substring(codigo from 5 for 2) = upper($1);
$_$;

CREATE FUNCTION quest_plantabase_numadmonnoocupados(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    plantabase varchar;
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   plantabase := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 5 for 2) = plantabase;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_numadmonnoocupados(character varying, character varying) IS 'Obtiene el nº de administraciones no ocupadas de un departamento SIGUANET en una planta  de la Universidad. 
SINTAXIS:
- select * from quest_plantabase_numadmonnoocupados(''B101'', ''PB'');
- select quest_plantabase_numadmonnoocupados(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_numadmonocupados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND substring(codigo from 5 for 2) = upper($1);
$_$;

CREATE FUNCTION quest_plantabase_numadmonocupados(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    plantabase varchar;
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   plantabase := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 5 for 2) = plantabase;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_numadmonocupados(character varying, character varying) IS 'Obtiene el nº de administraciones ocupadas de un departamento SIGUANET en una planta de la Universidad. 
SINTAXIS:
- select * from quest_plantabase_numadmonocupados(''B101'', ''PB'');
- select quest_plantabase_numadmonocupados(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_numbecarios(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM becarios WHERE substring(codigo from 5 for 2) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_plantabase_numbecarios(character varying) IS 'Obtiene el nº de becarios en una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantabase_numbecarios(''PB'');
- select quest_plantabase_numbecarios(''PB'');
';

CREATE FUNCTION quest_plantabase_numbecarios(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM becarios
	WHERE cod_depto_centro_subunidad = upper($1)
	AND substring(codigo FROM 5 FOR 2) = upper($2)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantabase_numbecarios(character varying, character varying) IS 'Obtiene el total de becarios de un departamento SIGUANET en una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantabase_numbecarios(''B101'', ''PB'');
- select quest_plantabase_numbecarios(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_numbecarios(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM becarios WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 5 for 2) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_plantabase_numbecarios(tipo character varying, denominacion character varying, planta character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM becarios WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 5 FOR 2) = ' || quote_literal(upper(planta)) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_numbecarios(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene el nº de BECARIOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad.
SINTAXIS:
- SELECT quest_plantabase_numbecarios(''crue'', ''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_numdespachosnoocupados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND substring(codigo from 5 for 2) = upper($1);
$_$;

CREATE FUNCTION quest_plantabase_numdespachosnoocupados(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    plantabase varchar;
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   plantabase := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 5 for 2) = plantabase;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_numdespachosnoocupados(character varying, character varying) IS 'Obtiene el nº de despachos no ocupados de un departamento SIGUANET en una planta de la Universidad. 
SINTAXIS:
- select * from quest_plantabase_numdespachosnoocupados(''B101'', ''PB'');
- select quest_plantabase_numdespachosnoocupados(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_numdespachosocupados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND substring(codigo from 5 for 2) = upper($1);
$_$;

CREATE FUNCTION quest_plantabase_numdespachosocupados(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    plantabase varchar;
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   plantabase := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 5 for 2) = plantabase;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_numdespachosocupados(character varying, character varying) IS 'Obtiene el nº de despachos ocupados de un departamento SIGUANET en una planta de la Universidad. 
SINTAXIS:
- select * from quest_plantabase_numdespachosocupados(''B101'', ''PB'');
- select quest_plantabase_numdespachosocupados(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_numestancias(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(codigo) FROM 
	(SELECT codigo from todasestancias where substring(codigo from 5 for 2) = upper($1) group by codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_plantabase_numestancias(character varying) IS 'Obtiene el nº de estancias en una planta de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantabase_numestancias(''PB'') ;
- select quest_plantabase_numestancias(''PB'');
';

CREATE FUNCTION quest_plantabase_numestancias(integer, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    cuenta int8;

BEGIN
   IF $1 NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', $1;
   ELSE
	SELECT count(codigo) INTO cuenta FROM 
       (SELECT codigo from todasestancias WHERE actividad = $1 AND substring(codigo from 5 for 2) = upper($2) GROUP BY codigo) AS foo;
      RETURN cuenta;
   END IF;
   
END
$_$;

COMMENT ON FUNCTION quest_plantabase_numestancias(integer, character varying) IS 'Obtiene el nº de estancias de una determinada actividad sigua en una planta de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantabase_numestancias(7, ''PB'');
- select quest_plantabase_numestancias(8, ''P1'');';

CREATE FUNCTION quest_plantabase_numestancias(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$DECLARE
    cuenta int8;
    adscripcion varchar;
    plantabase varchar;
BEGIN
   adscripcion := upper($1);
   plantabase := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

SELECT count(codigo) INTO cuenta FROM 
(SELECT t.codigo FROM todasestancias t, departamentossigua d 
WHERE t.coddpto = d.cod_dpto_sigua
AND t.coddpto = adscripcion 
AND substring(t.codigo from 5 for 2) = plantabase
GROUP BY t.codigo) AS foo;

RETURN cuenta;

END;
$_$;

COMMENT ON FUNCTION quest_plantabase_numestancias(character varying, character varying) IS 'Obtiene el nº de estancias de un departamento SIGUANET en una planta de la Universidad
SINTAXIS:
- SELECT quest_plantabase_numestancias(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_numestancias(integer, character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
   uso int4;
   adscripcion varchar;
   planta varchar;
   cuenta int8;
	
BEGIN
uso := $1;
adscripcion := upper($2);
planta := upper($3);
-- Control de errores
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;


   SELECT count(DISTINCT codigo) INTO cuenta FROM 
   todasestancias
   WHERE actividad = uso
   AND coddpto = adscripcion
   AND substring(codigo from 5 for 2) =  planta;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_numestancias(integer, character varying, character varying) IS 'Obtiene el nº de estancias de un departamento SIGUANET y una actividad en una planta de la Universidad.
SINTAXIS:
- SELECT * FROM quest_plantabase_numestancias(7,''B101'', ''PB'');
- SELECT quest_plantabase_numestancias(7,''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_numestancias(tipo character varying, denominacion character varying, planta character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
 cuenta bigint;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT count(codigo)  FROM 
           (SELECT t.codigo FROM todasestancias t, actividades a 
             WHERE substring(t.codigo from 5 for 2) = ' || quote_literal(upper(planta)) ||
             ' AND t.actividad = a.codactividad
               AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ' GROUP BY t.codigo) AS foo;' 
  INTO cuenta;
 RETURN cuenta;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_numestancias(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene el nº de estancias de un tipo de grupo de actividad (crue, u21,activresum) y una denominación de grupo de actividad en una planta de la universidad.
SINTAXIS
SELECT quest_plantabase_numestancias(''crue'',''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_numestancias(character varying, character varying, character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    agrupa varchar;
    denoactividad varchar;
    adscripcion varchar;
    planta varchar;
    cuenta int8;
	
BEGIN
agrupa := lower($1);
adscripcion := upper($3);
planta := upper($4);

IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
   RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
END IF;

IF agrupa = 'crue' THEN
	denoactividad := upper($2);
        IF denoactividad NOT IN ( SELECT crue FROM actividades) THEN
	   RAISE EXCEPTION 'La actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
		SELECT count(codigo) INTO cuenta FROM 
		(SELECT t.codigo FROM todasestancias t, departamentossigua d, actividades a 
		WHERE t.coddpto = d.cod_dpto_sigua
		AND t.actividad = a.codactividad
		AND t.coddpto = adscripcion 
		AND a.crue = denoactividad
		AND substring(t.codigo from 5 for 2) = planta
		GROUP BY t.codigo) AS foo;
		RETURN cuenta;
ELSEIF agrupa = 'u21' THEN
	denoactividad := upper($2);
        IF denoactividad NOT IN ( SELECT u21 FROM actividades) THEN
	   RAISE EXCEPTION 'La actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
		SELECT count(codigo) INTO cuenta FROM 
		(SELECT t.codigo FROM todasestancias t, departamentossigua d, actividades a 
		WHERE t.coddpto = d.cod_dpto_sigua
		AND t.actividad = a.codactividad
		AND t.coddpto = adscripcion 
		AND a.u21 = denoactividad
		AND substring(t.codigo from 5 for 2) = planta
		GROUP BY t.codigo) AS foo;
		RETURN cuenta;
ELSEIF agrupa = 'activresum' THEN
	denoactividad := $2;
        IF denoactividad NOT IN ( SELECT activresum FROM actividades) THEN
	   RAISE EXCEPTION 'La actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
		SELECT count(codigo) INTO cuenta FROM 
		(SELECT t.codigo FROM todasestancias t, departamentossigua d, actividades a 
		WHERE t.coddpto = d.cod_dpto_sigua
		AND t.actividad = a.codactividad
		AND t.coddpto = adscripcion 
		AND a.activresum = denoactividad
		AND substring(t.codigo from 5 for 2) = planta
		GROUP BY t.codigo) AS foo;
		RETURN cuenta;
ELSE
     RAISE exception 'No existe un campo valido con ese nombre: (%). Debe de ser u21, activresum o crue ', $1;
END IF;

END;

$_$;

COMMENT ON FUNCTION quest_plantabase_numestancias(character varying, character varying, character varying, character varying) IS 'Obtiene el nº de estancias de un departamento SIGUANET y un grupo de actividad en una planta de la Universidad.
SINTAXIS:
- SELECT * FROM quest_plantabase_numestancias(''crue'',''DOCENCIA'',''B101'',''PB'');';

CREATE FUNCTION quest_plantabase_numestanciasdocentes(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(codigo) FROM 
	(SELECT t.codigo from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE upper(a.activresum) = 'DOCENCIA'
         AND substring(t.codigo from 5 for 2) = $1
         group by t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_plantabase_numestanciasdocentes(character varying) IS 'Obtiene el nº de estancias de una planta de la Universidad cuya actividad pertenece al grupo "Docencia" de la tabla actividades. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantabase_numestanciasdocentes(''PB'');
- select quest_plantabase_numestanciasdocentes(''PB'');';

CREATE FUNCTION quest_plantabase_numestanciasdocentes(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(codigo) FROM 
	(SELECT t.codigo from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE upper(a.activresum) = 'DOCENCIA'
	 AND t.coddpto = upper($1)
         AND substring(t.codigo from 5 for 2) = $2
         group by t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_plantabase_numestanciasdocentes(character varying, character varying) IS 'Obtiene el nº de estancias adscritas a un departamento SIGUANET en una planta de la Universidad cuya actividad pertenece al grupo "Docencia" de la tabla actividades. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantabase_numestanciasdocentes(''B101'', ''PB'');
- select quest_plantabase_numestanciasdocentes(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_numestanciasnoocupadas(integer, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4;
  planta varchar;
  cuenta int8;
BEGIN
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;

        uso := $1;
        planta := upper($2);
	SELECT count(DISTINCT codigo) INTO cuenta
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = uso
        AND substring(codigo from 5 for 2) = planta;

	RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_numestanciasnoocupadas(integer, character varying) IS 'Obtiene el nº de estancias no ocupadas de una determinada actividad sigua en una planta de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantabase_numestanciasnoocupadas(50, ''PB'');
- select quest_plantabase_numestanciasnoocupadas(50, ''P1'');';

CREATE FUNCTION quest_plantabase_numestanciasnoocupadas(tipo character varying, denominacion character varying, planta character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
 cuenta bigint;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT count(DISTINCT t.codigo) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo NOT IN (SELECT codigo FROM todaspersonas)
             AND substring(t.codigo from 5 for 2) = ' || quote_literal(upper(planta)) || ';' INTO cuenta;
 RETURN cuenta;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_numestanciasnoocupadas(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene el nº de estancias no ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad.
SINTAXIS
SELECT quest_plantabase_numestanciasnoocupadas(''crue'',''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_numestanciasocupadas(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND substring(codigo from 5 for 2) = upper($1)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_plantabase_numestanciasocupadas(integer, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4;
  planta varchar;
  cuenta int8;
BEGIN
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;

        uso := $1;
        planta := upper($2);
	SELECT count(DISTINCT codigo) INTO cuenta
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = uso
        AND substring(codigo from 5 for 2) = planta;

	RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_numestanciasocupadas(integer, character varying) IS 'Obtiene el nº de estancias ocupadas de una determinada actividad sigua en una planta de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantabase_numestanciasocupadas(50, ''PB'');
- select quest_plantabase_numestanciasocupadas(50, ''P1'');';

CREATE FUNCTION quest_plantabase_numestanciasocupadas(integer[], character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4[];
  planta varchar;
  cuenta int8;
BEGIN
        uso := $1;
        planta := upper($2);
	SELECT count(DISTINCT codigo) INTO cuenta
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = ANY (uso)
        AND substring(codigo from 5 for 2) = planta;

	RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_numestanciasocupadas(integer[], character varying) IS 'Obtiene el nº de estancias ocupadas de una lista de actividades SIGUANET en una planta de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantabase_numestanciasocupadas(ARRAY[1,2,3,5], ''PB'');
- select quest_plantabase_numestanciasocupadas(ARRAY[1,2,3,5], ''P1'');';

CREATE FUNCTION quest_plantabase_numestanciasocupadas(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND coddpto = upper($1)
	AND substring(codigo from 5 for 2) = upper($2)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_plantabase_numestanciasocupadas(tipo character varying, denominacion character varying, planta character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
 cuenta bigint;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT count(DISTINCT t.codigo) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo IN (SELECT codigo FROM todaspersonas)
             AND substring(t.codigo from 5 for 2) = ' || quote_literal(upper(planta)) || ';' INTO cuenta;
 RETURN cuenta;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_numestanciasocupadas(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene el nº de estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad.
SINTAXIS
SELECT quest_plantabase_numestanciasocupadas(''crue'',''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_numestanciasutiles(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(*) FROM 
	(SELECT count(t.codigo) from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE a.util = true AND substring(codigo from 5 for 2) = upper($1) GROUP BY t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_plantabase_numestanciasutiles(character varying) IS 'Obtiene el nº de estancias útiles en una planta de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantabase_numestanciasutiles(''P1'');
- select quest_plantabase_numestanciasutiles(''P1'');
';

CREATE FUNCTION quest_plantabase_numestanciasutiles(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(*) FROM 
	(SELECT count(t.codigo)
	 FROM todasestancias t 
	 JOIN actividades a ON t.actividad = a.codactividad 
         WHERE a.util = true 
	 AND t.coddpto = upper($1)
	 AND substring(t.codigo from 5 for 2) = upper($2) GROUP BY t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_plantabase_numestanciasutiles(character varying, character varying) IS 'Obtiene el nº de estancias útiles adscritas a un departamento SIGUANET en una planta de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantabase_numestanciasutiles(''B101'', ''PB'');
- select quest_plantabase_numestanciasutiles(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_numexternos(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalexternos WHERE substring(codigo from 5 for 2) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_plantabase_numexternos(character varying) IS 'Obtiene el nº de externos en una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantabase_numexternos(''PB'');
- select quest_plantabase_numexternos(''PB'');
';

CREATE FUNCTION quest_plantabase_numexternos(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalexternos
	WHERE cod_dpto_sigua = upper($1)
	AND substring(codigo FROM 5 FOR 2) = upper($2)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantabase_numexternos(character varying, character varying) IS 'Obtiene el total de externos de un departamento SIGUANET en una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantabase_numexternos(''B101'', ''PB'');
- select quest_plantabase_numexternos(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_numexternos(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM personalexternos WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 5 for 2) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_plantabase_numexternos(tipo character varying, denominacion character varying, planta character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM personalexternos WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 5 FOR 2) = ' || quote_literal(upper(planta)) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_numexternos(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene el nº de empleados EXTERNOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad.
SINTAXIS:
- SELECT quest_plantabase_numexternos(''crue'', ''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_numpas(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalpas WHERE substring(codigo from 5 for 2) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_plantabase_numpas(character varying) IS 'Obtiene el nº de PAS en una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantabase_numpas(''PB'');
- select quest_plantabase_numpas(''PB'');
';

CREATE FUNCTION quest_plantabase_numpas(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalpas
	WHERE cod_unidad = upper($1)
	AND substring(codigo FROM 5 FOR 2) = upper($2)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantabase_numpas(character varying, character varying) IS 'Obtiene el total de PAS de un departamento SIGUANET en una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantabase_numpas(''B101'', ''PB'');
- select quest_plantabase_numpas(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_numpas(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM personalpas WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 5 for 2) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_plantabase_numpas(tipo character varying, denominacion character varying, planta character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM personalpas WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 5 FOR 2) = ' || quote_literal(upper(planta)) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_numpas(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene el nº de PAS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad.
SINTAXIS:
- SELECT quest_plantabase_numpas(''crue'', ''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_numpdi(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalpdi WHERE substring(codigo from 5 for 2) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_plantabase_numpdi(character varying) IS 'Obtiene el nº de PDI en una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantabase_numpdi(''PB'');
- select quest_plantabase_numpdi(''PB'');
';

CREATE FUNCTION quest_plantabase_numpdi(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalpdi
	WHERE cod_depto = upper($1)
	AND substring(codigo FROM 5 FOR 2) = upper($2)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantabase_numpdi(character varying, character varying) IS 'Obtiene el total de PAS de un departamento SIGUANET en una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantabase_numpdi(''B101'', ''PB'');
- select quest_plantabase_numpdi(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_numpdi(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM personalpdi WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 5 for 2) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_plantabase_numpdi(tipo character varying, denominacion character varying, planta character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM personalpdi WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 5 FOR 2) = ' || quote_literal(upper(planta)) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_numpdi(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene el nº de PDI ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad.
SINTAXIS:
- SELECT quest_plantabase_numpdi(''crue'', ''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_numpdicargos(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalpdi_cargos WHERE substring(codigo from 5 for 2) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_plantabase_numpdicargos(character varying) IS 'Obtiene el nº de PDI que desempeñan cargo en una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantabase_numpdicargos(''PB'');
- select quest_plantabase_numpdicargos(''PB'');
';

CREATE FUNCTION quest_plantabase_numpdicargos(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalpdi_cargos p
	JOIN cargos_dpto cd ON p.cod_cargo = cd.cod_cargo
	WHERE cd.coddpto = upper($1)
	AND substring(p.codigo from 5 for 2) = upper($2) 
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantabase_numpdicargos(character varying, character varying) IS 'Obtiene el nº de PDI que desempeñan cargo en un departamento sigua y en una planta de la Universidad . 
Se ejecuta de dos formas:
- select * from quest_plantabase_numpdicargos(''B101'', ''PB'');
- select quest_plantabase_numpdicargos(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_numpersonas(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(DISTINCT nif) FROM todaspersonas WHERE substring(codigo from 5 for 2) = upper($1) AND codigo != '0000PB997';$_$;

COMMENT ON FUNCTION quest_plantabase_numpersonas(character varying) IS 'Obtiene el nº de personas en una planta de la Universidad .
Se ejecuta de dos formas:
- select * from quest_plantabase_numpersonas(''PB'');
- select quest_plantabase_numpersonas(''PB'');
';

CREATE FUNCTION quest_plantabase_numpersonas(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$DECLARE
    cuenta int8;
    adscripcion varchar;
    plantabase varchar;
BEGIN
   adscripcion := upper($1);
   plantabase := upper($2);
   IF adscripcion NOT IN (SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT nif) INTO cuenta 
   FROM todaspersonas 
   WHERE cod_depto = adscripcion AND substring(codigo FROM 5 FOR 2) = plantabase AND codigo != '0000PB997';

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_numpersonas(character varying, character varying) IS 'Obtiene el nº de personas de un departamento sigua en una planta de la Universidad, indicando si es pas, pdi, becario, externo o alguna de sus combinaciones
SINTAXIS:
- SELECT quest_plantabase_numpersonas(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_numpersonas(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM todaspersonas WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 5 for 2) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_plantabase_numpersonas(tipo character varying, denominacion character varying, planta character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM todaspersonas WHERE codigo IN
            (SELECT codigo FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
            ' AND substring(codigo FROM 5 FOR 2) = ' || quote_literal(upper(planta)) || ');' INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_numpersonas(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene el nº de personas ubicadas en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad, en una planta de la universidad.
SINTAXIS:
- SELECT quest_plantabase_numpersonas(''crue'', ''DOCENCIA'');';

CREATE FUNCTION quest_plantabase_obteneradmonnoocupados(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias
WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
AND actividad = 8
AND enumplanta = $1;
$_$;

CREATE FUNCTION quest_plantabase_obteneradmonnoocupados(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND coddpto = upper($1)
	AND enumplanta  = upper($2);
$_$;

CREATE FUNCTION quest_plantabase_obteneradmonocupados(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias
WHERE codigo IN (SELECT codigo FROM todaspersonas)
AND actividad = 8
AND enumplanta = $1;
$_$;

CREATE FUNCTION quest_plantabase_obteneradmonocupados(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND coddpto = upper($1)
	AND enumplanta  = upper($2);
$_$;

CREATE FUNCTION quest_plantabase_obtenerdespachosnoocupados(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias
WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
AND actividad = 7
AND enumplanta = $1;
$_$;

CREATE FUNCTION quest_plantabase_obtenerdespachosnoocupados(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND coddpto = upper($1)
	AND enumplanta  = upper($2);
$_$;

CREATE FUNCTION quest_plantabase_obtenerdespachosocupados(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias
WHERE codigo IN (SELECT codigo FROM todaspersonas)
AND actividad = 7
AND enumplanta = $1;
$_$;

CREATE FUNCTION quest_plantabase_obtenerdespachosocupados(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND coddpto = upper($1)
	AND enumplanta  = upper($2);
$_$;

CREATE FUNCTION quest_plantabase_obtenerestancias(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila quest_estancias%ROWTYPE;
    adscripcion varchar;
    plantabase varchar;
BEGIN
   adscripcion := upper($1);
   plantabase := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   FOR fila IN 
	SELECT * FROM quest_estancias 
	WHERE coddpto = adscripcion
	AND enumplanta = plantabase
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_obtenerestancias(character varying, character varying) IS 'Obtiene las estancias de un tipo de grupo de actividad (crue, u21,activresum) y una denominación de grupo de actividad en una planta de la universidad.
SINTAXIS
SELECT * FROM quest_plantabase_obtenerestancias(''crue'',''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_obtenerestancias(integer, character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila quest_estancias%ROWTYPE;
    act integer;
    plantabase varchar;
BEGIN
   act := $1;
   plantabase := upper($2);
   IF act NOT IN (SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION '% no es una actividad en la tabla actividades.', act;
   END IF;

   FOR fila IN 
	SELECT * FROM quest_estancias 
	WHERE actividad = act
	AND enumplanta = plantabase
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_obtenerestancias(integer, character varying) IS 'Obtiene todas las estancias designadas para un uso SIGUANET en una planta de la Universidad
Ejemplo:
SELECT * FROM quest_plantabase_obtenerestancias(8, ''PB'');';

CREATE FUNCTION quest_plantabase_obtenerestancias(tipo character varying, denominacion character varying, planta character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $$
DECLARE
  validacion integer;
  c refcursor;
  estancia quest_estancias%ROWTYPE;
  tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
        WHEN 'activresum' THEN 'denogrupo'
        WHEN 'crue' THEN 'denocrue'
        WHEN 'u21' THEN 'denou21'
        END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_estancias WHERE substring(codigo from 5 for 2) = ' || quote_literal(upper(planta)) || ' AND lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || ';';
  LOOP
   FETCH c INTO estancia;
   EXIT WHEN NOT FOUND;
   RETURN NEXT estancia;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_obtenerestancias(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene las estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad.
SINTAXIS
SELECT quest_ua_obtenerestancias(''crue'', ''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_obtenerestanciasdocentes(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias 
	WHERE upper(denogrupo) = 'DOCENCIA'
	AND enumplanta = $1;
$_$;

CREATE FUNCTION quest_plantabase_obtenerestanciasdocentes(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias 
	WHERE upper(denogrupo) = 'DOCENCIA'
	AND coddpto = upper($1)
	AND enumplanta  = upper($2);
$_$;

CREATE FUNCTION quest_plantabase_obtenerestanciasnoocupadas(integer, character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4;
  planta varchar;
  fila quest_estancias%ROWTYPE;
BEGIN
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;

        uso := $1;
        planta := upper($2);
	FOR fila IN SELECT * FROM quest_estancias 
		    WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
		    AND actividad = uso
		    AND substring(codigo from 5 for 2) = planta
	LOOP
	  RETURN NEXT fila;
	END LOOP;
	
END;
$_$;

CREATE FUNCTION quest_plantabase_obtenerestanciasnoocupadas(tipo character varying, denominacion character varying, planta character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $$
DECLARE
 validacion integer;
 c refcursor;
 estancia quest_estancias%ROWTYPE;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 SELECT INTO tipo_ CASE lower(tipo)
  WHEN 'activresum' THEN 'denogrupo'
  WHEN 'crue' THEN 'denocrue'
  WHEN 'u21' THEN 'denou21'
 END;
 OPEN c FOR EXECUTE 'SELECT * FROM quest_estancias 
                      WHERE lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || 
                    ' AND codigo NOT IN (SELECT codigo FROM todaspersonas) 
                      AND substring(codigo from 5 for 2) = ' || quote_literal(upper(planta)) || ';' ;
 LOOP
  FETCH c INTO estancia;
  EXIT WHEN NOT FOUND;
  RETURN NEXT estancia;
 END LOOP;
 RETURN;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_obtenerestanciasnoocupadas(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene las estancias no ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad.
SINTAXIS
SELECT * FROM quest_plantabase_obtenerestanciasnoocupadas(''crue'',''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_obtenerestanciasocupadas(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias 
	WHERE (actividad <= 50 OR actividad = 99)
	AND codigo IN (SELECT codigo FROM todaspersonas)
	AND enumplanta = $1
	AND codigo != '0000PB997';

$_$;

CREATE FUNCTION quest_plantabase_obtenerestanciasocupadas(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias 
	WHERE (actividad <= 50 OR actividad = 99)
	AND codigo IN (SELECT codigo FROM todaspersonas)
	AND coddpto = upper($1)
	AND enumplanta = upper($2)
	AND codigo != '0000PB997';

$_$;

CREATE FUNCTION quest_plantabase_obtenerestanciasocupadas(integer, character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4;
  planta varchar;
  fila quest_estancias%ROWTYPE;
BEGIN
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;

        uso := $1;
        planta := upper($2);
	FOR fila IN SELECT * FROM quest_estancias 
		    WHERE codigo IN (SELECT codigo FROM todaspersonas)
		    AND actividad = uso
		    AND substring(codigo from 5 for 2) = planta
	LOOP
	  RETURN NEXT fila;
	END LOOP;
	
END;
$_$;

CREATE FUNCTION quest_plantabase_obtenerestanciasocupadas(tipo character varying, denominacion character varying, planta character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $$
DECLARE
 validacion integer;
 c refcursor;
 estancia quest_estancias%ROWTYPE;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 SELECT INTO tipo_ CASE lower(tipo)
  WHEN 'activresum' THEN 'denogrupo'
  WHEN 'crue' THEN 'denocrue'
  WHEN 'u21' THEN 'denou21'
 END;
 OPEN c FOR EXECUTE 'SELECT * FROM quest_estancias 
                      WHERE lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || 
                    ' AND codigo IN (SELECT codigo FROM todaspersonas) 
                      AND substring(codigo from 5 for 2) = ' || quote_literal(upper(planta)) || ';' ;
 LOOP
  FETCH c INTO estancia;
  EXIT WHEN NOT FOUND;
  RETURN NEXT estancia;
 END LOOP;
 RETURN;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_obtenerestanciasocupadas(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene las estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad.
SINTAXIS
SELECT * FROM quest_plantabase_obtenerestanciasocupadas(''crue'',''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_obtenerestanciasutiles(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila quest_estancias%ROWTYPE;
    adscripcion varchar;
    plantabase varchar;	
BEGIN
   adscripcion := upper($1);
   plantabase := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   FOR fila IN 
	SELECT * FROM quest_estancias t JOIN actividades a ON t.actividad = a.codactividad 
	WHERE a.util = true 
	AND t.coddpto = adscripcion
	AND enumplanta = plantabase
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_obtenerestanciasutiles(character varying, character varying) IS 'Obtiene todas las estancias útiles de un departamento sigua en una planta de la Universidad
Ejemplo:
SELECT * FROM quest_plantabase_obtenerestanciasutiles(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_obtenerplantasedificio(character varying) RETURNS SETOF quest_plantaedificio
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_plantasedificio() WHERE upper(planta) = upper($1) ORDER BY zona, edificio, indice;
$_$;

CREATE FUNCTION quest_plantabase_superficie(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
SELECT sum(st_area(geometria)) FROM todasestancias
WHERE substring(codigo from 5 for 2) = upper($1);
$_$;

COMMENT ON FUNCTION quest_plantabase_superficie(character varying) IS 'Obtiene la superficie total de una planta de la Universidad .';

CREATE FUNCTION quest_plantabase_superficie(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria)) 
	FROM todasestancias
	WHERE coddpto = upper($1)
	AND substring(codigo from 5 for 2) = upper($2);
$_$;

COMMENT ON FUNCTION quest_plantabase_superficie(character varying, character varying) IS 'Obtiene la superficie total de las estancias de un departamento SIGUANET en una planta de la Universidad.';

CREATE FUNCTION quest_plantabase_superficie(integer, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria)) 
	FROM todasestancias
	WHERE actividad = $1
	AND substring(codigo from 5 for 2) = upper($2);
$_$;

COMMENT ON FUNCTION quest_plantabase_superficie(integer, character varying) IS 'Obtiene la superficie total de las estancias designadas para un uso SIGUANET en una planta de la Universidad.';

CREATE FUNCTION quest_plantabase_superficie(tipo character varying, denominacion character varying, planta character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
 superficie double precision;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT sum(st_area(geometria)) FROM 
           (SELECT t.geometria FROM todasestancias t, actividades a 
             WHERE substring(t.codigo from 5 for 2) = ' || quote_literal(upper(planta)) ||
             ' AND t.actividad = a.codactividad
               AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ') AS foo;'
  INTO superficie;
 RETURN superficie;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_superficie(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene la superficie que ocupan las estancias de un tipo de grupo de actividad (crue, u21,activresum) y una denominación de grupo de actividad en una planta de la universidad.
SINTAXIS
SELECT quest_plantabase_superficie(''crue'',''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_superficieadmonnoocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    planta varchar;
BEGIN
   planta := upper($1);
   SELECT sum(st_area(t.geometria)) INTO sumag
   FROM todasestancias t WHERE t.codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND t.actividad = 8
   AND substring(codigo from 5 for 2) = planta;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_plantabase_superficieadmonnoocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    plantabase varchar;
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   plantabase := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 5 for 2) = plantabase;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieadmonnoocupados(character varying, character varying) IS 'Obtiene la superficie de administraciones no ocupadas de un departamento SIGUANET en una planta de la Universidad. 
SINTAXIS:
- select * from quest_plantabase_superficieadmonnoocupados(''B101'', ''PB'');
- select quest_plantabase_superficieadmonnoocupados(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_superficieadmonocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    planta varchar;
BEGIN
   planta := upper($1);
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND substring(codigo from 5 for 2) = planta;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_plantabase_superficieadmonocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    plantabase varchar;
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   plantabase := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 5 for 2) = plantabase;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieadmonocupados(character varying, character varying) IS 'Obtiene la superficie de administraciones ocupadas de un departamento SIGUANET en una planta de la Universidad. 
SINTAXIS:
- select * from quest_plantabase_superficieadmonocupados(''B101'', ''PB'');
- select quest_plantabase_superficieadmonocupados(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_superficiedespachosnoocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    planta varchar;
BEGIN
   planta := upper($1);
   SELECT sum(st_area(t.geometria)) INTO sumag
   FROM todasestancias t WHERE t.codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND t.actividad = 7
   AND substring(codigo from 5 for 2) = planta;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_plantabase_superficiedespachosnoocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    plantabase varchar;
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   plantabase := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 5 for 2) = plantabase;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_superficiedespachosnoocupados(character varying, character varying) IS 'Obtiene la superficie de despachos no ocupados de un departamento SIGUANET en una planta de la Universidad. 
SINTAXIS:
- select * from quest_plantabase_superficiedespachosnoocupados(''B101'', ''PB'');
- select quest_plantabase_superficiedespachosnoocupados(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_superficiedespachosocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    planta varchar;
BEGIN
   planta := upper($1);
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND substring(codigo from 5 for 2) = planta;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_plantabase_superficiedespachosocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    plantabase varchar;
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   plantabase := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 5 for 2) = plantabase;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_superficiedespachosocupados(character varying, character varying) IS 'Obtiene la superficie de despachos ocupados de un departamento SIGUANET en una planta de la Universidad. 
SINTAXIS:
- select * from quest_plantabase_superficiedespachosocupados(''B101'', ''PB'');
- select quest_plantabase_superficiedespachosocupados(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_superficiedocente(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE upper(a.activresum) = 'DOCENCIA'
        AND substring(t.codigo from 5 for 2) = $1;
$_$;

COMMENT ON FUNCTION quest_plantabase_superficiedocente(character varying) IS 'Obtiene la superficie total de las estancias docentes de una planta de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantabase_superficiedocente(''PB'');
- select quest_plantabase_superficiedocente(''PB'');';

CREATE FUNCTION quest_plantabase_superficiedocente(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE upper(a.activresum) = 'DOCENCIA'
	AND t.coddpto = upper($1)
        AND substring(t.codigo from 5 for 2) = upper($2);
$_$;

COMMENT ON FUNCTION quest_plantabase_superficiedocente(character varying, character varying) IS 'Obtiene la superficie total de las estancias docentes de una departamento SIGUANET en una planta de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantabase_superficiedocente(''B101'', ''PB'');
- select quest_plantabase_superficiedocente(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_superficieestanciasnoocupadas(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4;
  planta varchar;
  superficie float8;
BEGIN
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;

        uso := $1;
        planta := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = uso
        AND substring(codigo from 5 for 2) = planta;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieestanciasnoocupadas(integer, character varying) IS 'Obtiene la superficie de las estancias no ocupadas de una determinada actividad sigua en una planta de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantabase_superficieestanciasnoocupadas(50, ''PB'');
- select quest_plantabase_superficieestanciasnoocupadas(50, ''P1'');';

CREATE FUNCTION quest_plantabase_superficieestanciasnoocupadas(tipo character varying, denominacion character varying, planta character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
 superficie double precision;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT sum(st_area(t.geometria)) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo NOT IN (SELECT codigo FROM todaspersonas) 
             AND substring(t.codigo from 5 for 2) = ' || quote_literal(upper(planta)) || ';' INTO superficie;
 RETURN superficie;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_superficieestanciasnoocupadas(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene la superficie de las estancias no ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad.
SINTAXIS
SELECT quest_plantabase_superficieestanciasnoocupadas(''crue'',''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_superficieestanciasocupadas(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND substring(codigo from 5 for 2) = upper($1)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_plantabase_superficieestanciasocupadas(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4;
  planta varchar;
  superficie float8;
BEGIN
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;

        uso := $1;
        planta := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = uso
        AND substring(codigo from 5 for 2) = planta;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieestanciasocupadas(integer, character varying) IS 'Obtiene la superficie de las estancias ocupadas de una determinada actividad sigua en una planta de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantabase_superficieestanciasocupadas(50, ''PB'');
- select quest_plantabase_superficieestanciasocupadas(50, ''P1'');';

CREATE FUNCTION quest_plantabase_superficieestanciasocupadas(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4[];
  planta varchar;
  superficie float8;
BEGIN
        uso := $1;
        planta := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = ANY (uso)
        AND substring(codigo from 5 for 2) = planta;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieestanciasocupadas(integer[], character varying) IS 'Obtiene la superficie de las estancias ocupadas de una lista de actividades SIGUANET en una planta de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantabase_superficieestanciasocupadas(ARRAY[1,2,3,5], ''PB'');
- select quest_plantabase_superficieestanciasocupadas(ARRAY[1,2,3,5], ''P1'');';

CREATE FUNCTION quest_plantabase_superficieestanciasocupadas(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND coddpto = upper($1)
	AND substring(codigo from 5 for 2) = upper($2)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_plantabase_superficieestanciasocupadas(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4[];
  plantabase varchar;
  adscripcion varchar;
  superficie float8;
BEGIN
        uso := $1;
	adscripcion := upper($2);
        plantabase := upper($3);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = ANY (uso)
	AND coddpto = adscripcion
        AND substring(codigo from 5 for 2) = plantabase
	AND codigo != '0000PB997';

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieestanciasocupadas(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias ocupadas y adscritas a un departamento SIGUANET de una lista de actividades SIGUANET en una planta de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantabase_superficieestanciasocupadas(ARRAY[1,2,3,5], ''B101'', ''PB'');
- select quest_plantabase_superficieestanciasocupadas(ARRAY[1,2,3,5], ''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_superficieestanciasocupadas(tipo character varying, denominacion character varying, planta character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
 superficie double precision;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT sum(st_area(t.geometria)) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo IN (SELECT codigo FROM todaspersonas) 
             AND substring(t.codigo from 5 for 2) = ' || quote_literal(upper(planta)) || ';' INTO superficie;
 RETURN superficie;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_superficieestanciasocupadas(tipo character varying, denominacion character varying, planta character varying) IS 'Obtiene la superficie de las estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad.
SINTAXIS
SELECT quest_plantabase_superficieestanciasocupadas(''crue'',''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_superficieestanciasocupadasbec(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	planta varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        planta := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM becarios)
	AND actividad = $1
	AND substring(codigo from 5 for 2) = planta;
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieestanciasocupadasbec(integer, character varying) IS 'Obtiene la superficie de las estancias de una planta ocupadas por becarios de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantabase_superficieestanciasocupadasbecarios(8, ''PB'');
- SELECT quest_plantabase_superficieestanciasocupadasbecarios(8, ''PB'');
';

CREATE FUNCTION quest_plantabase_superficieestanciasocupadasbec(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	planta varchar;
	superficie float8;
BEGIN
        uso := $1;
        planta := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM becarios)
	AND actividad = ANY ($1)
	AND substring(codigo from 5 for 2) = planta;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieestanciasocupadasbec(integer[], character varying) IS 'Obtiene la superficie de las estancias de una planta ocupadas por becarios de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantabase_superficieestanciasocupadasbecarios(ARRAY[8,9,16],''P1'');
- SELECT quest_plantabase_superficieestanciasocupadasbecarios(ARRAY[8,9,16],''P1'');
';

CREATE FUNCTION quest_plantabase_superficieestanciasocupadasbec(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM becarios)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 5 for 2) = upper($3)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieestanciasocupadasbec(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por becarios de una lista de actividades SIGUANET y en una planta de la Universidad. 
SINTAXIS:
- SELECT * FROM quest_plantabase_superficieestanciasocupadasbec(ARRAY[8,9,16], ''B101'', ''PB'');
- SELECT quest_plantabase_superficieestanciasocupadasbec(ARRAY[8,9,16], ''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_superficieestanciasocupadasext(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	planta varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        planta := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalexternos)
	AND actividad = $1
	AND substring(codigo from 5 for 2) = planta;
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieestanciasocupadasext(integer, character varying) IS 'Obtiene la superficie de las estancias de una planta ocupadas por externos de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantabase_superficieestanciasocupadasexternos(8, ''PB'');
- SELECT quest_plantabase_superficieestanciasocupadasexternos(8, ''PB'');
';

CREATE FUNCTION quest_plantabase_superficieestanciasocupadasext(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	planta varchar;
	superficie float8;
BEGIN
        uso := $1;
        planta := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalexternos)
	AND actividad = ANY ($1)
	AND substring(codigo from 5 for 2) = planta;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieestanciasocupadasext(integer[], character varying) IS 'Obtiene la superficie de las estancias de una planta ocupadas por externos de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantabase_superficieestanciasocupadasexternos(ARRAY[8,9,16],''P1'');
- SELECT quest_plantabase_superficieestanciasocupadasexternos(ARRAY[8,9,16],''P1'');
';

CREATE FUNCTION quest_plantabase_superficieestanciasocupadasext(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalexternos)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 5 for 2) = upper($3)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieestanciasocupadasext(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por externos de una lista de actividades SIGUANET y en una planta de la Universidad. 
SINTAXIS:
- SELECT * FROM quest_plantabase_superficieestanciasocupadasext(ARRAY[8,9,16], ''B101'', ''PB'');
- SELECT quest_plantabase_superficieestanciasocupadasext(ARRAY[8,9,16], ''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_superficieestanciasocupadaspas(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	planta varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        planta := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpas)
	AND actividad = $1
	AND substring(codigo from 5 for 2) = planta;
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieestanciasocupadaspas(integer, character varying) IS 'Obtiene la superficie de las estancias de una planta ocupadas por PAS de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantabase_superficieestanciasocupadaspas(8, ''PB'');
- SELECT quest_plantabase_superficieestanciasocupadaspas(8, ''PB'');
';

CREATE FUNCTION quest_plantabase_superficieestanciasocupadaspas(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	planta varchar;
	superficie float8;
BEGIN
        uso := $1;
        planta := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpas)
	AND actividad = ANY ($1)
	AND substring(codigo from 5 for 2) = planta;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieestanciasocupadaspas(integer[], character varying) IS 'Obtiene la superficie de las estancias de una planta ocupadas por PAS de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantabase_superficieestanciasocupadaspas(ARRAY[8,9,16],''P1'');
- SELECT quest_plantabase_superficieestanciasocupadaspas(ARRAY[8,9,16],''P1'');
';

CREATE FUNCTION quest_plantabase_superficieestanciasocupadaspas(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpas)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 5 for 2) = upper($3)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieestanciasocupadaspas(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por PAS de una lista de actividades SIGUANET y en una planta de la Universidad. 
SINTAXIS:
- SELECT * FROM quest_plantabase_superficieestanciasocupadaspas(ARRAY[8,9,16], ''B101'', ''PB'');
- SELECT quest_plantabase_superficieestanciasocupadaspas(ARRAY[8,9,16], ''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_superficieestanciasocupadaspdi(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	planta varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        planta := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpdi)
	AND actividad = $1
	AND substring(codigo from 5 for 2) = planta;
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieestanciasocupadaspdi(integer, character varying) IS 'Obtiene la superficie de las estancias de una planta ocupadas por PDI de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantabase_superficieestanciasocupadaspdi(8, ''PB'');
- SELECT quest_plantabase_superficieestanciasocupadaspdi(8, ''PB'');
';

CREATE FUNCTION quest_plantabase_superficieestanciasocupadaspdi(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	planta varchar;
	superficie float8;
BEGIN
        uso := $1;
        planta := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpdi)
	AND actividad = ANY ($1)
	AND substring(codigo from 5 for 2) = planta;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieestanciasocupadaspdi(integer[], character varying) IS 'Obtiene la superficie de las estancias de una planta ocupadas por PDI de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantabase_superficieestanciasocupadaspdi(ARRAY[8,9,16],''P1'');
- SELECT quest_plantabase_superficieestanciasocupadaspdi(ARRAY[8,9,16],''P1'');
';

CREATE FUNCTION quest_plantabase_superficieestanciasocupadaspdi(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpdi)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 5 for 2) = upper($3)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieestanciasocupadaspdi(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por PDI de una lista de actividades SIGUANET y en una planta de la Universidad. 
SINTAXIS:
- SELECT * FROM quest_plantabase_superficieestanciasocupadaspdi(ARRAY[8,9,16], ''B101'', ''PB'');
- SELECT quest_plantabase_superficieestanciasocupadaspdi(ARRAY[8,9,16], ''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_superficieutil(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE a.util = true AND substring(t.codigo from 5 for 2) = upper($1);
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieutil(character varying) IS 'Obtiene la superficie útil de una planta de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantabase_superficieutil(''P1'');
- select quest_plantabase_superficieutil(''P1'');
';

CREATE FUNCTION quest_plantabase_superficieutil(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) 
	FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE a.util = true 
	AND t.coddpto = upper($1)
	AND substring(t.codigo from 5 for 2) = upper($2);
$_$;

COMMENT ON FUNCTION quest_plantabase_superficieutil(character varying, character varying) IS 'Obtiene la superficie útil de un departamento SIGUANET en una planta de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantabase_superficieutil(''B101'', ''PB'');
- select quest_plantabase_superficieutil(''B101'', ''PB'');';

CREATE FUNCTION quest_plantabase_ubicaciones(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE enumplanta = $1;
$_$;

COMMENT ON FUNCTION quest_plantabase_ubicaciones(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de una planta de la Universidad.
SINTAXIS
- SELECT * FROM quest_plantabase_ubicaciones(''PB'')';

CREATE FUNCTION quest_plantabase_ubicaciones(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT DISTINCT v.* FROM quest_ubicaciones v
	JOIN quest_personas2 p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE p.cod_depto = upper($1)
	AND v.enumplanta = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantabase_ubicaciones(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de las personas adscritas a un departamento SIGUANET en una planta de la Universidad.
SINTAXIS
- SELECT * FROM quest_plantabase_ubicaciones(''B101'', ''PB'')';

CREATE FUNCTION quest_plantabase_ubicaciones(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones WHERE actividad = $1 AND substring(codigo from 5 for 2) = $2;
$_$;

COMMENT ON FUNCTION quest_plantabase_ubicaciones(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en una planta concreta de la universidad.
SINTAXIS
- SELECT * FROM quest_plantabase_ubicaciones(7, ''PB'')';

CREATE FUNCTION quest_plantabase_ubicaciones(tipo character varying, denominacion character varying, planta character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND substring(codigo FROM 5 FOR 2) = ' || quote_literal(upper(planta)) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_ubicaciones(tipo character varying, denominacion character varying, planta character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para las personas ubicadas en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad, en una planta de la universidad.
SINTAXIS:
- SELECT * FROM quest_plantabase_ubicaciones(''crue'', ''DOCENCIA'');';

CREATE FUNCTION quest_plantabase_ubicacionesbecarios(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones
   WHERE esbecario = true 
   AND locbecario = true
   and reftbl = 'becarios'
   AND (actividad <= 50 OR actividad = 99)
   AND enumplanta = $1;
$_$;

COMMENT ON FUNCTION quest_plantabase_ubicacionesbecarios(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de una planta de la Universidad.
La condición where matiza que sólo devuelve estancias con BECARIOS
SINTAXIS
- SELECT * FROM quest_plantabase_ubicacionesbecarios(''PB'')';

CREATE FUNCTION quest_plantabase_ubicacionesbecarios(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_becarios p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.esbecario = true 
	AND v.locbecario = true
	AND v.reftbl = 'becarios'
	AND p.cod_depto_centro_subunidad = upper($1)
	AND v.enumplanta = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantabase_ubicacionesbecarios(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociación becario/estancia de los becarios adscritos a un departamento SIGUANET ubicados en una planta de la Universidad.
SINTAXIS
- SELECT * FROM quest_plantabase_ubicacionesbecarios(''B101'', ''PB'')';

CREATE FUNCTION quest_plantabase_ubicacionesbecarios(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 
   AND substring(codigo from 5 for 2) = $2 
   AND esbecario = true 
   AND locbecario = true
   AND reftbl = 'becarios';
$_$;

COMMENT ON FUNCTION quest_plantabase_ubicacionesbecarios(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion becario/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en una planta de la universidad.
SINTAXIS
- SELECT * FROM quest_plantabase_ubicacionesbecarios(7, ''PB'')';

CREATE FUNCTION quest_plantabase_ubicacionesbecarios(tipo character varying, denominacion character varying, planta character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND esbecario = true 
                       AND locbecario = true
                       AND reftbl = ''becarios''
                       AND substring(codigo FROM 5 FOR 2) = ' || quote_literal(upper(planta)) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_ubicacionesbecarios(tipo character varying, denominacion character varying, planta character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los BECARIOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad.
SINTAXIS:
- SELECT * FROM quest_plantabase_ubicacionesbecarios(''crue'', ''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_ubicacionescargos(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE espdicargo = true 
   AND locpdicargo = true
   AND reftbl = 'personalpdi_cargos'
   AND (actividad <= 50 OR actividad = 99)
   AND enumplanta = $1;
$_$;

COMMENT ON FUNCTION quest_plantabase_ubicacionescargos(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de una planta de la Universidad.
La condición where matiza que sólo devuelve estancias con CARGOS
SINTAXIS
- SELECT * FROM quest_plantabase_ubicacionescargos(''PB'')';

CREATE FUNCTION quest_plantabase_ubicacionescargos(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalpdi_cargos p ON v.nif = p.nif AND v.codigo = p.codigo
	JOIN cargos_dpto cd ON p.cod_cargo = cd.cod_cargo
	WHERE v.espdicargo = true 
	AND v.locpdicargo = true
	AND v.reftbl = 'personalpdi_cargos'
	AND cd.coddpto = upper($1)
	AND v.enumplanta = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantabase_ubicacionescargos(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociación pdicargo/estancia de los cargos adscritos a un departamento SIGUANET ubicados en una planta de la Universidad.
SINTAXIS
- SELECT * FROM quest_plantabase_ubicacionescargos(''B101'', ''PB'')';

CREATE FUNCTION quest_plantabase_ubicacionesexternos(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones
   WHERE esexterno = true
   AND locexterno = true
   AND reftbl = 'personalexternos'
   AND (actividad <= 50 OR actividad = 99)
   AND enumplanta = $1;
$_$;

COMMENT ON FUNCTION quest_plantabase_ubicacionesexternos(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de una planta de la Universidad.
La condición where matiza que sólo devuelve estancias DE EXTERNOS
SINTAXIS
- SELECT * FROM quest_plantabase_ubicacionesexternos(''PB'')';

CREATE FUNCTION quest_plantabase_ubicacionesexternos(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalexternos p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.esexterno = true 
	AND v.locexterno = true
	AND v.reftbl = 'personalexternos'
	AND p.cod_dpto_sigua = upper($1)
	AND v.enumplanta = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantabase_ubicacionesexternos(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion externo/estancia de los externos adscritos a un departamento SIGUANET ubicados en una planta de la Universidad.
SINTAXIS
- SELECT * FROM quest_plantabase_ubicacionesexternos(''B101'', ''PB'')';

CREATE FUNCTION quest_plantabase_ubicacionesexternos(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 
   AND substring(codigo from 5 for 2) = $2 
   AND esexterno = true 
   AND locexterno = true
   AND reftbl = 'personalexternos';
$_$;

COMMENT ON FUNCTION quest_plantabase_ubicacionesexternos(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion externo/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en una planta de la universidad.
SINTAXIS
- SELECT * FROM quest_plantabase_ubicacionesexternos(7, ''PB'')';

CREATE FUNCTION quest_plantabase_ubicacionesexternos(tipo character varying, denominacion character varying, planta character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND esexterno = true 
                       AND locexterno = true
                       AND reftbl = ''personalexternos''
                       AND substring(codigo FROM 5 FOR 2) = ' || quote_literal(upper(planta)) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_ubicacionesexternos(tipo character varying, denominacion character varying, planta character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los empleadoS EXTERNOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad.
SINTAXIS:
- SELECT * FROM quest_plantabase_ubicacionesexternos(''crue'', ''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_ubicacionespas(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_ubicaciones 
	WHERE espas = true
	AND locpas = true
	AND reftbl = 'personalpas'
	AND (actividad <= 50 OR actividad = 99)
	AND enumplanta = $1;
$_$;

COMMENT ON FUNCTION quest_plantabase_ubicacionespas(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de una planta de la Universidad.
La condición where matiza que sólo devuelve estancias con pas
SINTAXIS
- SELECT * FROM quest_plantabase_ubicacionespas(''PB'')';

CREATE FUNCTION quest_plantabase_ubicacionespas(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalpas p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.espas = true 
	AND v.locpas = true
	AND v.reftbl = 'personalpas'
	AND p.cod_unidad = upper($1)
	AND v.enumplanta = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantabase_ubicacionespas(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion pas/estancia de los pas adscritos a un departamento SIGUANET ubicados en una planta de la Universidad.
La condición where matiza que sólo devuelve estancias con pas
SINTAXIS
- SELECT * FROM quest_plantabase_ubicacionespas(''B101'', ''PB'')';

CREATE FUNCTION quest_plantabase_ubicacionespas(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 
   AND substring(codigo from 5 for 2) = $2 
   AND espas = true 
   AND locpas = true
   AND reftbl = 'personalpas';
$_$;

COMMENT ON FUNCTION quest_plantabase_ubicacionespas(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion pas/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en una planta de la universidad.
SINTAXIS
- SELECT * FROM quest_plantabase_ubicacionespas(7, ''PB'')';

CREATE FUNCTION quest_plantabase_ubicacionespas(tipo character varying, denominacion character varying, planta character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND espas = true 
                       AND locpas = true
                       AND reftbl = ''personalpas''
                       AND substring(codigo FROM 5 FOR 2) = ' || quote_literal(upper(planta)) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_ubicacionespas(tipo character varying, denominacion character varying, planta character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los PAS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad.
SINTAXIS:
- SELECT * FROM quest_plantabase_ubicacionespas(''crue'', ''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantabase_ubicacionespdi(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE espdi = true 
   AND locpdi = true
   AND reftbl = 'personalpdi'
   AND (actividad <= 50 OR actividad = 99)
   AND enumplanta = $1;
$_$;

COMMENT ON FUNCTION quest_plantabase_ubicacionespdi(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de una planta de la Universidad.
La condición where matiza que sólo devuelve estancias con PDI
SINTAXIS
- SELECT * FROM quest_plantabase_ubicacionespdi(''PB'')';

CREATE FUNCTION quest_plantabase_ubicacionespdi(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalpdi p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.espdi = true 
	AND v.locpdi = true
	AND v.reftbl = 'personalpdi'
	AND p.cod_depto = upper($1)
	AND v.enumplanta = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantabase_ubicacionespdi(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociación pdi/estancia de los pdi adscritos a un departamento SIGUANET ubicados en una planta de la Universidad.
SINTAXIS
- SELECT * FROM quest_plantabase_ubicacionespdi(''B101'', ''PB'')';

CREATE FUNCTION quest_plantabase_ubicacionespdi(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 
   AND substring(codigo from 5 for 2) = $2 
   AND espdi = true 
   AND locpdi = true
   AND reftbl = 'personalpdi';
$_$;

COMMENT ON FUNCTION quest_plantabase_ubicacionespdi(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion pdi/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en una planta de la universidad.
SINTAXIS
- SELECT * FROM quest_plantabase_ubicacionespdi(7, ''PB'')';

CREATE FUNCTION quest_plantabase_ubicacionespdi(tipo character varying, denominacion character varying, planta character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND espdi = true 
                       AND locpdi = true
                       AND reftbl = ''personalpdi''
                       AND substring(codigo FROM 5 FOR 2) = ' || quote_literal(upper(planta)) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantabase_ubicacionespdi(tipo character varying, denominacion character varying, planta character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los PDI ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de la universidad.
SINTAXIS:
- SELECT * FROM quest_plantabase_ubicacionespdi(''crue'', ''DOCENCIA'', ''PB'');';

CREATE FUNCTION quest_plantazona_densidad(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantazona_superficieestanciasocupadas(ARRAY[4,5,7,8,9,16], upper($1), upper($2)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE actividad = ANY (ARRAY[4,5,7,8,9,16])
										       AND substring(codigo from 1 for 6) LIKE upper($1 || '__' || $2)));
$_$;

COMMENT ON FUNCTION quest_plantazona_densidad(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en un campus/sede y una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantazona_densidad(''00'', ''P1'');
- select quest_plantazona_densidad(''00'', ''P1'');
';

CREATE FUNCTION quest_plantazona_densidad(character varying, character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantazona_superficieestanciasocupadas(ARRAY[4,5,7,8,9,16], $1, $2, $3) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE actividad = ANY (ARRAY[4,5,7,8,9,16])
						     AND coddpto = upper($1)
						     AND substring(codigo from 1 for 6) LIKE upper($2 || '__' || $3)));
$_$;

COMMENT ON FUNCTION quest_plantazona_densidad(character varying, character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad, en una planta de un campus o sede. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_plantazona_densidad(''crue'', ''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_densidad(integer, character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_plantazona_superficieestanciasocupadas($1, $2, $3) INTO superficie;
 SELECT count(nif) INTO poblacion FROM todaspersonas WHERE codigo IN (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 2) = $2 AND substring(codigo from 5 for 2) = $3);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_densidad(integer, character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en las estancias de una determinada actividad SIGUANET para una planta de un campus o sede de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantazona_densidad(4, ''00'', ''PB'');
- select quest_plantazona_densidad(8, ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_densidad(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  SELECT quest_plantazona_superficieestanciasocupadas(tipo, denominacion, zona, planta) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM todaspersonas WHERE codigo IN
            (SELECT codigo FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
          '   AND substring(codigo FROM 1 FOR 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;   
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

CREATE FUNCTION quest_plantazona_densidadbecarios(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantazona_superficieestanciasocupadasbec(ARRAY[4,5,7], upper($1), upper($2)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM becarios)
										       AND actividad = ANY (ARRAY[4,5,7])
										       AND substring(codigo from 1 for 6) LIKE upper($1 || '__' || $2)));
$_$;

COMMENT ON FUNCTION quest_plantazona_densidadbecarios(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por becarios en un campus/sede y una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantazona_densidadbecarios(''00'', ''P1'');
- select quest_plantazona_densidadbecarios(''00'', ''P1'');
';

CREATE FUNCTION quest_plantazona_densidadbecarios(character varying, character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantazona_superficieestanciasocupadasbec(ARRAY[4,5,7], $1, $2, $3) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM becarios)
						     AND actividad = ANY (ARRAY[4,5,7])
						     AND coddpto = upper($1)
						     AND substring(codigo from 1 for 6) LIKE upper($2 || '__' || $3)));
$_$;

COMMENT ON FUNCTION quest_plantazona_densidadbecarios(character varying, character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por becarios en un departamento SIGUANET y en una planta de campus/sede de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantazona_densidadbecarios(''B101'', ''00'', ''PB'');
- select quest_plantazona_densidadbecarios(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_densidadbecarios(integer, character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_plantazona_superficieestanciasocupadasbec($1, $2, $3) INTO superficie;
 SELECT count(nif) INTO poblacion FROM becarios WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM becarios)
                                           AND actividad = $1
                                           AND substring(codigo from 1 for 2) = $2 AND substring(codigo from 5 for 2) = $3);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_densidadbecarios(integer, character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por becario en las estancias de una determinada actividad SIGUANET para una planta de un campus o sede. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantazona_densidadbecarios(4, ''00'', ''P1'');
- select quest_plantazona_densidadbecarios(8, ''00'', ''P1'');';

CREATE FUNCTION quest_plantazona_densidadbecarios(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_plantazona_superficieestanciasocupadasbec(actlist, zona, planta) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM becarios WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_densidadbecarios(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Obtiene los m2 de espacio de trabajo por BECARIO en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_plantazona_densidadbecarios(''crue'', ''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_densidadexternos(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantazona_superficieestanciasocupadasext(ARRAY[7,8], upper($1), upper($2)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalexternos)
										       AND actividad = ANY (ARRAY[7,8])
										       AND substring(codigo from 1 for 6) LIKE upper($1 || '__' || $2)));
$_$;

COMMENT ON FUNCTION quest_plantazona_densidadexternos(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por externo en un campus/sede y una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantazona_densidadexternos(''00'', ''P1'');
- select quest_plantazona_densidadexternos(''00'', ''P1'');
';

CREATE FUNCTION quest_plantazona_densidadexternos(character varying, character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantazona_superficieestanciasocupadasext(ARRAY[7,8], $1, $2, $3) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM personalexternos)
						     AND actividad = ANY (ARRAY[7,8])
						     AND coddpto = upper($1)
						     AND substring(codigo from 1 for 6) LIKE upper($2 || '__' || $3)));
$_$;

COMMENT ON FUNCTION quest_plantazona_densidadexternos(character varying, character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por externos en un departamento SIGUANET y en una planta de un campus/sede de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantazona_densidadexternos(''B101'', ''00'', ''PB'');
- select quest_plantazona_densidadexternos(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_densidadexternos(integer, character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_plantazona_superficieestanciasocupadasext($1, $2, $3) INTO superficie;
 SELECT count(nif) INTO poblacion FROM personalexternos WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalexternos)
                                           AND actividad = $1
                                           AND substring(codigo from 1 for 2) = $2 AND substring(codigo from 5 for 2) = $3);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_densidadexternos(integer, character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por externo en las estancias de una determinada actividad SIGUANET para una planta de un campus o sede. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantazona_densidadexternos(4, ''00'', ''P1'');
- select quest_plantazona_densidadexternos(8, ''00'', ''P1'');';

CREATE FUNCTION quest_plantazona_densidadexternos(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_plantazona_superficieestanciasocupadasext(actlist, zona, planta) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM personalexternos WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_densidadexternos(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado EXTERNO en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_plantazona_densidadexternos(''crue'', ''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_densidadpas(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantazona_superficieestanciasocupadaspas(ARRAY[4,5,8,9,16], upper($1), upper($2)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpas)
										       AND actividad = ANY (ARRAY[4,5,8,9,16])
										       AND substring(codigo from 1 for 6) LIKE upper($1 || '__' || $2)));
$_$;

COMMENT ON FUNCTION quest_plantazona_densidadpas(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por PAS en un campus/sede y una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantazona_densidadpas(''00'', ''P1'');
- select quest_plantazona_densidadpas(''00'', ''P1'');
';

CREATE FUNCTION quest_plantazona_densidadpas(character varying, character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantazona_superficieestanciasocupadaspas(ARRAY[4,5,8,9,16], $1, $2, $3) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM personalpas)
						     AND actividad = ANY (ARRAY[4,5,8,9,16])
						     AND coddpto = upper($1)
						     AND substring(codigo from 1 for 6) LIKE upper($2 || '__' || $3)));
$_$;

COMMENT ON FUNCTION quest_plantazona_densidadpas(character varying, character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por PAS en un departamento SIGUANET y en una planta de un campus/sede de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantazona_densidadpas(''B101'', ''00'', ''PB'');
- select quest_plantazona_densidadpas(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_densidadpas(integer, character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_plantazona_superficieestanciasocupadaspas($1, $2, $3) INTO superficie;
 SELECT count(nif) INTO poblacion FROM personalpas WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpas)
                                           AND actividad = $1
                                           AND substring(codigo from 1 for 2) = $2 AND substring(codigo from 5 for 2) = $3);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_densidadpas(integer, character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado PAS en las estancias de una determinada actividad SIGUANET para una planta de un campus o sede. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantazona_densidadpas(4, ''00'', ''P1'');
- select quest_plantazona_densidadpas(8, ''00'', ''P1'');';

CREATE FUNCTION quest_plantazona_densidadpas(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_plantazona_superficieestanciasocupadaspas(actlist, zona, planta) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM personalpas WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_densidadpas(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado tipo PAS en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_plantazona_densidadpas(''crue'', ''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_densidadpdi(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantazona_superficieestanciasocupadaspdi(ARRAY[4,5,7], upper($1), upper($2)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpdi)
										       AND actividad = ANY (ARRAY[4,5,7])
										       AND substring(codigo from 1 for 6) LIKE upper($1 || '__' || $2)));
$_$;

COMMENT ON FUNCTION quest_plantazona_densidadpdi(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por PDI en un campus/sede y una planta de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantazona_densidadpdi(''00'', ''P1'');
- select quest_plantazona_densidadpdi(''00'', ''P1'');
';

CREATE FUNCTION quest_plantazona_densidadpdi(character varying, character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_plantazona_superficieestanciasocupadaspdi(ARRAY[4,5,7], $1, $2, $3) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM personalpdi)
						     AND actividad = ANY (ARRAY[4,5,7])
						     AND coddpto = upper($1)
						     AND substring(codigo from 1 for 6) LIKE upper($2 || '__' || $3)));
$_$;

COMMENT ON FUNCTION quest_plantazona_densidadpdi(character varying, character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por PDI en un departamento SIGUANET y en una planta de un campus/sede de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantazona_densidadpdi(''B101'', ''00'', ''PB'');
- select quest_plantazona_densidadpdi(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_densidadpdi(integer, character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_plantazona_superficieestanciasocupadaspdi($1, $2, $3) INTO superficie;
 SELECT count(nif) INTO poblacion FROM personalpdi WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpdi)
                                           AND actividad = $1
                                           AND substring(codigo from 1 for 2) = $2 AND substring(codigo from 5 for 2) = $3);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_densidadpdi(integer, character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado pdi en las estancias de una determinada actividad SIGUANET para una planta de un campus o sede. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_plantazona_densidadpdi(4, ''00'', ''P1'');
- select quest_plantazona_densidadpdi(8, ''00'', ''P1'');';

CREATE FUNCTION quest_plantazona_densidadpdi(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_plantazona_superficieestanciasocupadaspdi(actlist, zona, planta) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM personalpdi WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_densidadpdi(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado tipo PDI en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_plantazona_densidadpdi(''crue'', ''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numadmonnoocupados(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND substring(codigo from 1 for 6) LIKE upper($1) || '__' || upper($2);
$_$;

COMMENT ON FUNCTION quest_plantazona_numadmonnoocupados(character varying, character varying) IS 'Obtiene el nº de administraciones no ocupadas de un departamento SIGUANET en una planta de un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_plantazona_numadmonnoocupados(''B101'', ''00'', ''PB'');
- select quest_plantazona_numadmonnoocupados(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numadmonnoocupados(character varying, character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    zona varchar;
    planta varchar;
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   planta := upper($3);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 6) LIKE zona || '__' || planta;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_numadmonnoocupados(character varying, character varying, character varying) IS 'Obtiene el nº de administraciones no ocupadas de un departamento SIGUANET en una planta de campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_plantazona_numadmonnoocupados(''B101'',  ''00'', ''PB'');
- select quest_plantazona_numadmonnoocupados(''B101'',  ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numadmonocupados(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND substring(codigo from 1 for 6) LIKE upper($1) || '__' || upper($2);
$_$;

CREATE FUNCTION quest_plantazona_numadmonocupados(character varying, character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    zona varchar;
    planta varchar;
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   planta := upper($3);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 6) LIKE zona || '__' || planta;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_numadmonocupados(character varying, character varying, character varying) IS 'Obtiene el nº de administraciones ocupadas de un departamento SIGUANET en una planta de un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_plantazona_numadmonocupados(''B101'',  ''00'', ''PB'');
- select quest_plantazona_numadmonocupados(''B101'',  ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numbecarios(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM becarios WHERE substring(codigo from 1 for 6) LIKE upper($1 || '__' || $2) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_plantazona_numbecarios(character varying, character varying) IS 'Obtiene el nº de becarios en un campus/sede y una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantazona_numbecarios(''00'', ''PB'');
- select quest_plantazona_numbecarios(''00'', ''PB'');
';

CREATE FUNCTION quest_plantazona_numbecarios(character varying, character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM becarios
	WHERE cod_depto_centro_subunidad = upper($1)
	AND substring(codigo from 1 for 6) LIKE upper($2 || '__' || $3)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantazona_numbecarios(character varying, character varying, character varying) IS 'Obtiene el total de becarios de un departamento SIGUANET en una planta de un campus/sede de la Universidad. 
Se ejecuta de dos formas:
- SELECT * FROM quest_plantazona_numbecarios(''B101'', ''00'', ''PB'');
- SELECT quest_plantazona_numbecarios(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numbecarios(integer, character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM becarios WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 2) = $2 AND substring(codigo from 5 for 2) = $3)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_plantazona_numbecarios(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM becarios WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_numbecarios(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Obtiene el nº de BECARIOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede.
SINTAXIS:
- SELECT quest_plantazona_numbecarios(''crue'', ''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numdespachosnoocupados(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND substring(codigo from 1 for 6) LIKE upper($1) || '__' || upper($2);
$_$;

CREATE FUNCTION quest_plantazona_numdespachosnoocupados(character varying, character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    zona varchar;
    planta varchar;
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   planta := upper($3);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 6) LIKE zona || '__' || planta;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_numdespachosnoocupados(character varying, character varying, character varying) IS 'Obtiene el nº de despachos no ocupados de un departamento SIGUANET en una planta de un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_plantazona_numdespachosnoocupados(''B101'',  ''00'', ''PB'');
- select quest_plantazona_numdespachosnoocupados(''B101'',  ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numdespachosocupados(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND substring(codigo from 1 for 6) LIKE upper($1) || '__' || upper($2);
$_$;

CREATE FUNCTION quest_plantazona_numdespachosocupados(character varying, character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    zona varchar;
    planta varchar;
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   planta := upper($3);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 6) LIKE zona || '__' || planta;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_numdespachosocupados(character varying, character varying, character varying) IS 'Obtiene el nº de despachos ocupados de un departamento SIGUANET en una planta de un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_plantazona_numdespachosocupados(''B101'',  ''00'', ''PB'');
- select quest_plantazona_numdespachosocupados(''B101'',  ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numestancias(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(codigo) FROM 
	(SELECT codigo from todasestancias where substring(codigo from 1 for 6) like upper($1 || '__' || $2) group by codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_plantazona_numestancias(character varying, character varying) IS 'Obtiene el nº de estancias en un campus/sede y una planta de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantazona_numestancias(''00'',''PB'') ;
- select quest_plantazona_numestancias(''00'',''PB'');
';

CREATE FUNCTION quest_plantazona_numestancias(character varying, character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    cuenta int8;
    adscripcion varchar;
    zona varchar;
    planta varchar;
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   planta := upper($3);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

	SELECT count(codigo) INTO cuenta FROM 
	(SELECT t.codigo FROM todasestancias t, departamentossigua d 
	WHERE t.coddpto = d.cod_dpto_sigua
	AND t.coddpto = adscripcion 
	AND substring(t.codigo from 1 for 6) LIKE zona || '__' || planta
	GROUP BY t.codigo) AS foo;

RETURN cuenta;

END;
$_$;

COMMENT ON FUNCTION quest_plantazona_numestancias(character varying, character varying, character varying) IS 'Obtiene el nº de estancias de un departamento SIGUANET en una planta de un campus/sede de la Universidad
SINTAXIS:
- SELECT quest_plantazona_numestancias(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numestancias(integer, character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    uso int4;
    zona varchar;
    planta varchar;
    cuenta int8;

BEGIN
   uso := $1;
   zona := $2;
   planta := $3;
   IF $1 NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   ELSE
	SELECT count(codigo) INTO cuenta FROM 
       (SELECT codigo from todasestancias WHERE actividad = uso AND substring(codigo from 1 for 6) LIKE upper(zona || '__' || planta) GROUP BY codigo) AS foo;
      RETURN cuenta;
   END IF;
   
END
$_$;

COMMENT ON FUNCTION quest_plantazona_numestancias(integer, character varying, character varying) IS 'Obtiene el nº de estancias de una determinada actividad sigua en un campus/sede y una planta de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantazona_numestancias(7, ''00'', ''PB'');
- select quest_plantazona_numestancias(8, ''00'', ''P1'');';

CREATE FUNCTION quest_plantazona_numestancias(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
 cuenta bigint;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT count(codigo)  FROM 
           (SELECT t.codigo FROM todasestancias t, actividades a 
             WHERE substring(t.codigo from 1 for 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) ||
             ' AND t.actividad = a.codactividad
               AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ' GROUP BY t.codigo) AS foo;' 
  INTO cuenta;
 RETURN cuenta;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_numestancias(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Obtiene el nº de estancias de un tipo de grupo de actividad (crue, u21,activresum) y una denominación de grupo de actividad en una planta de un campus o sede.
SINTAXIS
SELECT quest_plantazona_numestancias(''crue'',''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numestancias(integer, character varying, character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
   uso int4;
   adscripcion varchar;
   zona varchar;
   planta varchar;
   cuenta int8;
	
BEGIN
uso := $1;
adscripcion := upper($2);
zona := $3;
planta := $4;
-- Control de errores
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta FROM 
   todasestancias
   WHERE actividad = uso
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 6) LIKE upper(zona || '__' || planta);

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_numestancias(integer, character varying, character varying, character varying) IS 'Obtiene el nº de estancias de un departamento SIGUANET y una actividad en un campus/sede y una planta de la Universidad.
SINTAXIS:
- SELECT * FROM quest_plantazona_numestancias(7,''B101'', ''00'', ''PB'');
- SELECT quest_plantazona_numestancias(7,''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numestancias(character varying, character varying, character varying, character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    agrupa varchar;
    denoactividad varchar;
    adscripcion varchar;
    zona varchar;
    planta varchar;
    cuenta int8;
	
BEGIN
agrupa := lower($1);
adscripcion := upper($3);
zona := upper($4);
planta := upper($5);

IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
   RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
END IF;

IF agrupa = 'crue' THEN
	denoactividad := upper($2);
        IF denoactividad NOT IN ( SELECT crue FROM actividades) THEN
	   RAISE EXCEPTION 'La actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
		SELECT count(codigo) INTO cuenta FROM 
		(SELECT t.codigo FROM todasestancias t, departamentossigua d, actividades a 
		WHERE t.coddpto = d.cod_dpto_sigua
		AND t.actividad = a.codactividad
		AND t.coddpto = adscripcion 
		AND a.crue = denoactividad
		AND substring(t.codigo from 1 for 6) LIKE upper(zona || '__' || planta)
		GROUP BY t.codigo) AS foo;
		RETURN cuenta;
ELSEIF agrupa = 'u21' THEN
	denoactividad := upper($2);
        IF denoactividad NOT IN ( SELECT u21 FROM actividades) THEN
	   RAISE EXCEPTION 'La actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
		SELECT count(codigo) INTO cuenta FROM 
		(SELECT t.codigo FROM todasestancias t, departamentossigua d, actividades a 
		WHERE t.coddpto = d.cod_dpto_sigua
		AND t.actividad = a.codactividad
		AND t.coddpto = adscripcion 
		AND a.u21 = denoactividad
		AND substring(t.codigo from 1 for 6) LIKE upper(zona || '__' || planta)
		GROUP BY t.codigo) AS foo;
		RETURN cuenta;
ELSEIF agrupa = 'activresum' THEN
	denoactividad := $2;
        IF denoactividad NOT IN ( SELECT activresum FROM actividades) THEN
	   RAISE EXCEPTION 'La actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
		SELECT count(codigo) INTO cuenta FROM 
		(SELECT t.codigo FROM todasestancias t, departamentossigua d, actividades a 
		WHERE t.coddpto = d.cod_dpto_sigua
		AND t.actividad = a.codactividad
		AND t.coddpto = adscripcion 
		AND a.activresum = denoactividad
		AND substring(t.codigo from 1 for 6) LIKE upper(zona || '__' || planta)
		GROUP BY t.codigo) AS foo;
		RETURN cuenta;
ELSE
     RAISE exception 'No existe un campo valido con ese nombre: (%). Debe de ser u21, activresum o crue ', $1;
END IF;

END;

$_$;

COMMENT ON FUNCTION quest_plantazona_numestancias(character varying, character varying, character varying, character varying, character varying) IS 'Obtiene el nº de estancias de un departamento SIGUANET y un grupo de actividad en un campus/sede y una planta de la Universidad.
SINTAXIS:
- SELECT * FROM quest_plantazona_numestancias(''crue'',''DOCENCIA'',''B101'',''00'',''PB'');';

CREATE FUNCTION quest_plantazona_numestanciasdocentes(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(codigo) FROM 
	(SELECT t.codigo from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE upper(a.activresum) = 'DOCENCIA'
         AND substring(codigo from 1 for 6) LIKE $1 || '__' || $2
         group by t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_plantazona_numestanciasdocentes(character varying, character varying) IS 'Obtiene el nº de estancias de una planta de un campus/sede de la Universidad cuya actividad pertenece al grupo "Docencia" de la tabla actividades. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantazona_numestanciasdocentes(''00'', ''PB'');
- select quest_plantazona_numestanciasdocentes(''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numestanciasdocentes(character varying, character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(codigo) FROM 
	(SELECT t.codigo from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE upper(a.activresum) = 'DOCENCIA'
	 AND t.coddpto = upper($1)
         AND substring(t.codigo from 1 for 6) LIKE upper($2 || '__' || $3)
         group by t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_plantazona_numestanciasdocentes(character varying, character varying, character varying) IS 'Obtiene el nº de estancias adscritas a un departamento SIGUANET en una planta de un campus/sede de la Universidad cuya actividad pertenece al grupo "Docencia" de la tabla actividades. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantazona_numestanciasdocentes(''B101'', ''00'', ''PB'');
- select quest_plantazona_numestanciasdocentes(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numestanciasnoocupadas(integer, character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1
	AND substring(codigo from 1 for 6) LIKE upper($2 || '__' || $3);
$_$;

CREATE FUNCTION quest_plantazona_numestanciasnoocupadas(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
 cuenta bigint;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT count(DISTINCT t.codigo) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo NOT IN (SELECT codigo FROM todaspersonas)
             AND substring(t.codigo from 1 for 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ';' INTO cuenta;
 RETURN cuenta;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_numestanciasnoocupadas(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Obtiene el nº de estancias no ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede.
SINTAXIS
SELECT quest_plantazona_numestanciasnoocupadas(''crue'',''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numestanciasocupadas(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND codigo != '0000PB997'
	AND substring(codigo from 1 for 6) LIKE upper($1) || '__' || upper($2);
$_$;

CREATE FUNCTION quest_plantazona_numestanciasocupadas(character varying, character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND coddpto = upper($1)
	AND substring(codigo from 1 for 6) LIKE upper($2 || '__' || $3)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_plantazona_numestanciasocupadas(integer, character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1
	AND substring(codigo from 1 for 6) LIKE upper($2 || '__' || $3);
$_$;

CREATE FUNCTION quest_plantazona_numestanciasocupadas(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
 cuenta bigint;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT count(DISTINCT t.codigo) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo IN (SELECT codigo FROM todaspersonas)
             AND substring(t.codigo from 1 for 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ';' INTO cuenta;
 RETURN cuenta;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_numestanciasocupadas(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Obtiene el nº de estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede.
SINTAXIS
SELECT quest_plantazona_numestanciasocupadas(''crue'',''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numestanciasutiles(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(*) FROM 
	(SELECT count(t.codigo) from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE a.util = true AND substring(codigo from 1 for 6) LIKE upper($1 || '__' || $2) GROUP BY t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_plantazona_numestanciasutiles(character varying, character varying) IS 'Obtiene el nº de estancias útiles en un campus/sede y una planta de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantazona_numestanciasutiles(''00'', ''P1'');
- select quest_plantazona_numestanciasutiles(''00'', ''P1'');
';

CREATE FUNCTION quest_plantazona_numestanciasutiles(character varying, character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(*) FROM 
	(SELECT count(t.codigo)
	 FROM todasestancias t 
	 JOIN actividades a ON t.actividad = a.codactividad 
         WHERE a.util = true 
	 AND t.coddpto = upper($1)
	 AND substring(t.codigo from 1 for 6) LIKE upper($2 || '__' || $3)
	 GROUP BY t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_plantazona_numestanciasutiles(character varying, character varying, character varying) IS 'Obtiene el nº de estancias útiles adscritas a un departamento SIGUANET en una planta de un campus/sede de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantazona_numestanciasutiles(''B101'', ''00'', ''PB'');
- select quest_plantazona_numestanciasutiles(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numexternos(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalexternos WHERE substring(codigo from 1 for 6) LIKE upper($1 || '__' || $2) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_plantazona_numexternos(character varying, character varying) IS 'Obtiene el nº de externos en un campus/sede y una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantazona_numexternos(''00'', ''PB'');
- select quest_plantazona_numexternos(''00'', ''PB'');
';

CREATE FUNCTION quest_plantazona_numexternos(character varying, character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalexternos
	WHERE cod_dpto_sigua = upper($1)
	AND substring(codigo from 1 for 6) LIKE upper($2 || '__' || $3)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantazona_numexternos(character varying, character varying, character varying) IS 'Obtiene el total de externos de un departamento SIGUANET en una planta de un campus/sede de la Universidad. 
Se ejecuta de dos formas:
- SELECT * FROM quest_plantazona_numexternos(''B101'', ''00'', ''PB'');
- SELECT quest_plantazona_numexternos(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numexternos(integer, character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM personalexternos WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 2) = $2 AND substring(codigo from 5 for 2) = $3)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_plantazona_numexternos(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM personalexternos WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_numexternos(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Obtiene el nº de empleados EXTERNOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede.
SINTAXIS:
- SELECT quest_plantazona_numexternos(''crue'', ''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numpas(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalpas WHERE substring(codigo from 1 for 6) LIKE upper($1 || '__' || $2) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_plantazona_numpas(character varying, character varying) IS 'Obtiene el nº de PAS en un campus/sede y una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantazona_numpas(''00'', ''PB'');
- select quest_plantazona_numpas(''00'', ''PB'');
';

CREATE FUNCTION quest_plantazona_numpas(character varying, character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalpas
	WHERE cod_unidad = upper($1)
	AND substring(codigo from 1 for 6) LIKE upper($2 || '__' || $3)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantazona_numpas(character varying, character varying, character varying) IS 'Obtiene el total de PAS de un departamento SIGUANET en una planta de campus/sede de la Universidad. 
Se ejecuta de dos formas:
- SELECT * FROM quest_plantazona_numpas(''B101'', ''00'', ''PB'');
- SELECT quest_plantazona_numpas(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numpas(integer, character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM personalpas WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 2) = $2 AND substring(codigo from 5 for 2) = $3)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_plantazona_numpas(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM personalpas WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_numpas(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Obtiene el nº de PAS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede.
SINTAXIS:
- SELECT quest_plantazona_numpas(''crue'', ''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numpdi(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalpdi WHERE substring(codigo from 1 for 6) LIKE upper($1 || '__' || $2) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_plantazona_numpdi(character varying, character varying) IS 'Obtiene el nº de PDI en un campus/sede y una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantazona_numpdi(''00'', ''PB'');
- select quest_plantazona_numpdi(''00'', ''PB'');
';

CREATE FUNCTION quest_plantazona_numpdi(character varying, character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalpdi
	WHERE cod_depto = upper($1)
	AND substring(codigo from 1 for 6) LIKE upper($2 || '__' || $3)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantazona_numpdi(character varying, character varying, character varying) IS 'Obtiene el total de PDI de un departamento SIGUANET en una planta de campus/sede de la Universidad. 
Se ejecuta de dos formas:
- SELECT * FROM quest_plantazona_numpdi(''B101'', ''00'', ''PB'');
- SELECT quest_plantazona_numpdi(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numpdi(integer, character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM personalpdi WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 2) = $2 AND substring(codigo from 5 for 2) = $3)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_plantazona_numpdi(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM personalpdi WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_numpdi(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Obtiene el nº de PDI ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede.
SINTAXIS:
- SELECT quest_plantazona_numpdi(''crue'', ''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numpdicargos(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalpdi_cargos WHERE substring(codigo from 1 for 6) LIKE upper($1 || '__' || $2) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_plantazona_numpdicargos(character varying, character varying) IS 'Obtiene el nº de PDI que desempeñan cargo en un campus/sede y una planta de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_plantazona_numpdicargos(''00'', ''PB'');
- select quest_plantazona_numpdicargos(''00'', ''PB'');
';

CREATE FUNCTION quest_plantazona_numpdicargos(character varying, character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalpdi_cargos p
	JOIN cargos_dpto cd ON p.cod_cargo = cd.cod_cargo
	WHERE cd.coddpto = upper($1)
	AND substring(p.codigo from 1 for 6) LIKE upper($2 || '__' || $3)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantazona_numpdicargos(character varying, character varying, character varying) IS 'Obtiene el nº de PDI que desempeñan cargo en un departamento sigua y en una planta de campus/sede de la Universidad. 
Se ejecuta de dos formas:
- SELECT * FROM quest_plantazona_numpdicargos(''B101'', ''00'', ''PB'');
- SELECT quest_plantazona_numpdicargos(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numpersonas(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
     SELECT count(DISTINCT nif) FROM todaspersonas
     WHERE substring (codigo from 1 for 6) LIKE upper($1 || '__' || $2) AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantazona_numpersonas(character varying, character varying) IS 'Obtiene el nº de personas en un campus/sede y una planta de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantazona_numpersonas(''00'', ''PB'');
- select quest_plantazona_numpersonas(''00'', ''PB'');
';

CREATE FUNCTION quest_plantazona_numpersonas(character varying, character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$DECLARE
    cuenta int8;
    adscripcion varchar;
    zona varchar;
    planta varchar;
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   planta := upper($3);
   IF adscripcion NOT IN (SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT nif) INTO cuenta 
   FROM todaspersonas
   WHERE cod_depto = upper(adscripcion)
   AND substring(codigo from 1 for 6) LIKE zona || '__' || planta
   AND codigo != '0000PB997';

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_numpersonas(character varying, character varying, character varying) IS 'Obtiene el nº de personas de un departamento sigua en una planta de un campus o sede, indicando si es pas, pdi, becario, externo o alguna de sus combinaciones
SINTAXIS:
- SELECT quest_plantazona_numpersonas(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_numpersonas(integer, character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM todaspersonas WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 2) = $2 AND substring(codigo from 5 for 2) = $3)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_plantazona_numpersonas(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM todaspersonas WHERE codigo IN
            (SELECT codigo FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
            ' AND substring(codigo FROM 1 FOR 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ');' INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

CREATE FUNCTION quest_plantazona_obteneradmonnoocupados(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias
WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
AND actividad = 8
AND codzona = $1
AND enumplanta = $2;
$_$;

CREATE FUNCTION quest_plantazona_obteneradmonnoocupados(character varying, character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND coddpto = upper($1)
	AND codzona = upper($2)
	AND enumplanta = upper($3);
$_$;

CREATE FUNCTION quest_plantazona_obteneradmonocupados(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias
WHERE codigo IN (SELECT codigo FROM todaspersonas)
AND actividad = 8
AND codzona = $1
AND enumplanta = $2;
$_$;

CREATE FUNCTION quest_plantazona_obteneradmonocupados(character varying, character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND coddpto = upper($1)
	AND codzona = upper($2)
	AND enumplanta = upper($3);
$_$;

CREATE FUNCTION quest_plantazona_obtenerdespachosnoocupados(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias
WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
AND actividad = 7
AND codzona = $1
AND enumplanta = $2;
$_$;

CREATE FUNCTION quest_plantazona_obtenerdespachosnoocupados(character varying, character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND coddpto = upper($1)
	AND codzona = upper($2)
	AND enumplanta = upper($3);
$_$;

CREATE FUNCTION quest_plantazona_obtenerdespachosocupados(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias
WHERE codigo IN (SELECT codigo FROM todaspersonas)
AND actividad = 7
AND codzona = $1
AND enumplanta = $2;
$_$;

CREATE FUNCTION quest_plantazona_obtenerdespachosocupados(character varying, character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND coddpto = upper($1)
	AND codzona = upper($2)
	AND enumplanta = upper($3);
$_$;

CREATE FUNCTION quest_plantazona_obtenerestancias(character varying, character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila quest_estancias%ROWTYPE;
    adscripcion varchar;
    zona varchar;
    planta varchar;
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   planta := upper($3);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   FOR fila IN 
	SELECT * FROM quest_estancias 
	WHERE coddpto = adscripcion
	AND codzona = zona
	AND enumplanta = planta
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_obtenerestancias(character varying, character varying, character varying) IS 'Obtiene todas las estancias de un departamento sigua en una planta de un campus/sede de la Universidad
Ejemplo:
- select * from quest_plantazona_obtenerestancias(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_obtenerestancias(integer, character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila quest_estancias%ROWTYPE;
    act integer;
    zona varchar;
    planta varchar;
BEGIN
   act := $1;
   zona := upper($2);
   planta := upper($3);
   IF act NOT IN (SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION '% no es una actividad en la tabla actividades.', act;
   END IF;

   FOR fila IN 
	SELECT * FROM quest_estancias 
	WHERE actividad = act
	AND codzona = zona
	AND enumplanta = planta
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_obtenerestancias(integer, character varying, character varying) IS 'Obtiene todas las estancias designadas para un uso SIGUANET en una planta de un campus/sede de la Universidad
Ejemplo:
- select * from quest_plantazona_obtenerestancias(8, ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_obtenerestancias(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $$
DECLARE
  validacion integer;
  c refcursor;
  estancia quest_estancias%ROWTYPE;
  tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
        WHEN 'activresum' THEN 'denogrupo'
        WHEN 'crue' THEN 'denocrue'
        WHEN 'u21' THEN 'denou21'
        END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_estancias WHERE substring(codigo from 1 for 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ' AND lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || ';';  LOOP
   FETCH c INTO estancia;
   EXIT WHEN NOT FOUND;
   RETURN NEXT estancia;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_obtenerestancias(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Obtiene las estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede.
SINTAXIS
SELECT quest_ua_obtenerestancias(''crue'',''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_obtenerestanciasdocentes(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias 
	WHERE upper(denogrupo) = 'DOCENCIA'
	AND codzona = $1
	AND enumplanta = $2;
$_$;

CREATE FUNCTION quest_plantazona_obtenerestanciasdocentes(character varying, character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias 
	WHERE upper(denogrupo) = 'DOCENCIA'
	AND coddpto = upper($1)
	AND codzona = upper($2)
	AND enumplanta = upper($3);
$_$;

CREATE FUNCTION quest_plantazona_obtenerestanciasnoocupadas(integer, character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1
	AND codzona = upper($2)
	AND enumplanta = upper($3);

$_$;

CREATE FUNCTION quest_plantazona_obtenerestanciasnoocupadas(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $$
DECLARE
 validacion integer;
 c refcursor;
 estancia quest_estancias%ROWTYPE;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 SELECT INTO tipo_ CASE lower(tipo)
  WHEN 'activresum' THEN 'denogrupo'
  WHEN 'crue' THEN 'denocrue'
  WHEN 'u21' THEN 'denou21'
 END;
 OPEN c FOR EXECUTE 'SELECT * FROM quest_estancias 
                      WHERE lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || 
                    ' AND codigo NOT IN (SELECT codigo FROM todaspersonas) 
                      AND substring(codigo from 1 for 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ';' ;
 LOOP
  FETCH c INTO estancia;
  EXIT WHEN NOT FOUND;
  RETURN NEXT estancia;
 END LOOP;
 RETURN;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_obtenerestanciasnoocupadas(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Obtiene las estancias no ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede.
SINTAXIS
SELECT * FROM quest_plantazona_obtenerestanciasnoocupadas(''crue'',''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_obtenerestanciasocupadas(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias 
	WHERE (actividad <= 50 OR actividad = 99)
	AND codigo IN (SELECT codigo FROM todaspersonas)
	AND codigo != '0000PB997'
	AND codzona = $1
	AND enumplanta = $2;

$_$;

CREATE FUNCTION quest_plantazona_obtenerestanciasocupadas(character varying, character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias 
	WHERE (actividad <= 50 OR actividad = 99)
	AND codigo IN (SELECT codigo FROM todaspersonas)
	AND coddpto = upper($1)
	AND codzona = upper($2)
	AND enumplanta = upper($3)
	AND codigo != '0000PB997';

$_$;

CREATE FUNCTION quest_plantazona_obtenerestanciasocupadas(integer, character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1
	AND codzona = upper($2)
	AND enumplanta = upper($3);

$_$;

CREATE FUNCTION quest_plantazona_obtenerestanciasocupadas(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $$
DECLARE
 validacion integer;
 c refcursor;
 estancia quest_estancias%ROWTYPE;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 SELECT INTO tipo_ CASE lower(tipo)
  WHEN 'activresum' THEN 'denogrupo'
  WHEN 'crue' THEN 'denocrue'
  WHEN 'u21' THEN 'denou21'
 END;
 OPEN c FOR EXECUTE 'SELECT * FROM quest_estancias 
                      WHERE lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || 
                    ' AND codigo IN (SELECT codigo FROM todaspersonas) 
                      AND substring(codigo from 1 for 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ';' ;
 LOOP
  FETCH c INTO estancia;
  EXIT WHEN NOT FOUND;
  RETURN NEXT estancia;
 END LOOP;
 RETURN;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_obtenerestanciasocupadas(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Obtiene las estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede.
SINTAXIS
SELECT * FROM quest_plantazona_obtenerestanciasocupadas(''crue'',''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_obtenerestanciasutiles(character varying, character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila quest_estancias%ROWTYPE;
    adscripcion varchar;
    zona varchar;
    planta varchar;
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   planta := upper($3);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   FOR fila IN 
	SELECT * FROM quest_estancias t JOIN actividades a ON t.actividad = a.codactividad 
	WHERE a.util = true 
	AND t.coddpto = adscripcion
	AND t.codzona = zona
	AND t.enumplanta = planta
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_obtenerestanciasutiles(character varying, character varying, character varying) IS 'Obtiene todas las estancias útiles de un departamento sigua en una planta de campus/sede de la Universidad
Ejemplo:
- select * from quest_plantazona_obtenerestanciasutiles(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_obtenerplantasedificio(character varying, character varying) RETURNS SETOF quest_plantaedificio
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_plantasedificio() WHERE zona = $1  AND upper(planta) = upper($2) ORDER BY zona, edificio, indice;
$_$;

CREATE FUNCTION quest_plantazona_superficie(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
SELECT sum(st_area(geometria)) FROM todasestancias
WHERE substring(codigo from 1 for 6) like upper($1 || '__' || $2);
$_$;

COMMENT ON FUNCTION quest_plantazona_superficie(character varying, character varying) IS 'Obtiene la superficie total de un campus/sede y una planta de la Universidad.';

CREATE FUNCTION quest_plantazona_superficie(character varying, character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria)) 
	FROM todasestancias
	WHERE coddpto = upper($1)
	AND substring(codigo from 1 for 6) LIKE upper($2 || '__' || $3);
$_$;

COMMENT ON FUNCTION quest_plantazona_superficie(character varying, character varying, character varying) IS 'Obtiene la superficie total de las estancias de un departamento SIGUANET en una planta de un campus/sede de la Universidad.';

CREATE FUNCTION quest_plantazona_superficie(integer, character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria)) 
	FROM todasestancias
	WHERE actividad = $1
	AND substring(codigo from 1 for 6) LIKE upper($2 || '__' || $3);
$_$;

COMMENT ON FUNCTION quest_plantazona_superficie(integer, character varying, character varying) IS 'Obtiene la superficie total de las estancias designado para un uso SIGUANET en una planta de un campus/sede de la Universidad.';

CREATE FUNCTION quest_plantazona_superficie(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
 superficie double precision;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT sum(st_area(geometria)) FROM 
           (SELECT t.geometria FROM todasestancias t, actividades a 
             WHERE substring(t.codigo from 1 for 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) ||
             ' AND t.actividad = a.codactividad
               AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ') AS foo;'
  INTO superficie;
 RETURN superficie;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_superficie(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Obtiene la superficie que ocupan las estancias de un tipo de grupo de actividad (crue, u21,activresum) y una denominación de grupo de actividad en una planta de un campus o sede.
SINTAXIS
SELECT quest_plantazona_superficie(''crue'',''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_superficieadmonnoocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    zona varchar;
    planta varchar;
BEGIN
   zona := upper($1);
   planta := upper($2);
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND substring(codigo from 1 for 6) LIKE zona || '__' || planta;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_plantazona_superficieadmonnoocupados(character varying, character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    zona varchar;
    planta varchar;
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   planta := upper($3);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 6) LIKE zona || '__' || planta;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieadmonnoocupados(character varying, character varying, character varying) IS 'Obtiene la superficie de administraciones no ocupadas de un departamento SIGUANET en una planta de campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_plantazona_superficieadmonnoocupados(''B101'', ''00'', ''PB'');
- select quest_plantazona_superficieadmonnoocupados(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_superficieadmonocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    zona varchar;
    planta varchar;
BEGIN
   zona := upper($1);
   planta := upper($2);
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND substring(codigo from 1 for 6) LIKE zona || '__' || planta;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_plantazona_superficieadmonocupados(character varying, character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    zona varchar;
    planta varchar;
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   planta := upper($3);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 6) LIKE zona || '__' || planta;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieadmonocupados(character varying, character varying, character varying) IS 'Obtiene la superficie de administraciones ocupadas de un departamento SIGUANET en una planta de un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_plantazona_superficieadmonocupados(''B101'', ''00'', ''PB'');
- select quest_plantazona_superficieadmonocupados(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_superficiedespachosnoocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    zona varchar;
    planta varchar;
BEGIN
   zona := upper($1);
   planta := upper($2);
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND substring(codigo from 1 for 6) LIKE zona || '__' || planta;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_plantazona_superficiedespachosnoocupados(character varying, character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    zona varchar;
    planta varchar;
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   planta := upper($3);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 6) LIKE zona || '__' || planta;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_superficiedespachosnoocupados(character varying, character varying, character varying) IS 'Obtiene la superficie de despachos no ocupados de un departamento SIGUANET en una planta de un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_plantazona_superficiedespachosnoocupados(''B101'', ''00'', ''PB'');
- select quest_plantazona_superficiedespachosnoocupados(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_superficiedespachosocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    zona varchar;
    planta varchar;
BEGIN
   zona := upper($1);
   planta := upper($2);
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND substring(codigo from 1 for 6) LIKE zona || '__' || planta;

   RETURN sumag;

END;
$_$;

COMMENT ON FUNCTION quest_plantazona_superficiedespachosocupados(character varying, character varying) IS 'Obtiene la superficie de despachos ocupados de un departamento SIGUANET en una planta de un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_plantazona_superficiedespachosocupados(''B101'', ''00'', ''PB'');
- select quest_plantazona_superficiedespachosocupados(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_superficiedespachosocupados(character varying, character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    zona varchar;
    planta varchar;
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   planta := upper($3);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 6) LIKE zona || '__' || planta;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_superficiedespachosocupados(character varying, character varying, character varying) IS 'Obtiene la superficie de despachos ocupados de un departamento SIGUANET en una planta de un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_plantazona_superficiedespachosocupados(''B101'', ''00'', ''PB'');
- select quest_plantazona_superficiedespachosocupados(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_superficiedocente(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE upper(a.activresum) = 'DOCENCIA'
        AND substring(codigo from 1 for 6) LIKE $1 || '__' || $2;
$_$;

COMMENT ON FUNCTION quest_plantazona_superficiedocente(character varying, character varying) IS 'Obtiene la superficie total de las estancias docentes de una planta de un campus/sede de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantazona_superficiedocente(''00'', ''PB'');
- select quest_plantazona_superficiedocente(''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_superficiedocente(character varying, character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE upper(a.activresum) = 'DOCENCIA'
	AND t.coddpto = upper($1)
        AND substring(t.codigo from 1 for 6) LIKE upper($2 || '__' || $3);
$_$;

COMMENT ON FUNCTION quest_plantazona_superficiedocente(character varying, character varying, character varying) IS 'Obtiene la superficie total de las estancias docentes de una departamento SIGUANET en una planta de un campus/sede de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantazona_superficiedocente(''B101'', ''00'', ''PB'');
- select quest_plantazona_superficiedocente(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_superficieestanciasnoocupadas(integer, character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4;
  zona varchar;
  planta varchar;
  superficie float8;
BEGIN
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;

        uso := $1;
        zona := $2;
        planta := $3;
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = uso
        AND substring(codigo from 1 for 6) LIKE upper(zona || '__' || planta);

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieestanciasnoocupadas(integer, character varying, character varying) IS 'Obtiene la superficie de las estancias no ocupadas de una determinada actividad SIGUANET en un campus/sede y una planta de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantazona_superficieestanciasnoocupadas(50, ''00'', ''PB'');
- select quest_plantazona_superficieestanciasnoocupadas(50, ''00'', ''P1'');';

CREATE FUNCTION quest_plantazona_superficieestanciasnoocupadas(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
 superficie double precision;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT sum(st_area(t.geometria)) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo NOT IN (SELECT codigo FROM todaspersonas) 
             AND substring(t.codigo from 1 for 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ';' INTO superficie;
 RETURN superficie;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_superficieestanciasnoocupadas(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Obtiene la superficie de las estancias no ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede.
SINTAXIS
SELECT quest_plantazona_superficieestanciasnoocupadas(''crue'',''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_superficieestanciasocupadas(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND codigo != '0000PB997'
	AND substring(codigo from 1 for 6) LIKE upper($1) || '__' || upper($2);
$_$;

CREATE FUNCTION quest_plantazona_superficieestanciasocupadas(integer, character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4;
  zona varchar;
  planta varchar;
  superficie float8;
BEGIN
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;

        uso := $1;
        zona := $2;
        planta := $3;
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = uso
        AND substring(codigo from 1 for 6) LIKE upper(zona || '__' || planta);

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieestanciasocupadas(integer, character varying, character varying) IS 'Obtiene la superficie de las estancias ocupadas de una determinada actividad SIGUANET en un campus/sede y una planta de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantazona_superficieestanciasocupadas(50, ''00'', ''PB'');
- select quest_plantazona_superficieestanciasocupadas(50, ''00'', ''P1'');';

CREATE FUNCTION quest_plantazona_superficieestanciasocupadas(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4[];
  zona varchar;
  planta varchar;
  superficie float8;
BEGIN
        uso := $1;
        zona := $2;
        planta := $3;
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = ANY (uso)
        AND substring(codigo from 1 for 6) LIKE upper(zona || '__' || planta);

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieestanciasocupadas(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias ocupadas de una lista de actividades SIGUANET en un campus/sede y una planta de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantazona_superficieestanciasocupadas(ARRAY[1,2,3,5], ''00'', ''PB'');
- select quest_plantazona_superficieestanciasocupadas(ARRAY[1,2,3,5], ''00'', ''P1'');';

CREATE FUNCTION quest_plantazona_superficieestanciasocupadas(character varying, character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND coddpto = upper($1)
	AND substring(codigo from 1 for 6) LIKE upper($2 || '__' || $3)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_plantazona_superficieestanciasocupadas(integer[], character varying, character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4[];
  zona varchar;
  planta varchar;
  adscripcion varchar;
  superficie float8;
BEGIN
        uso := $1;
	adscripcion := upper($2);
        zona := upper($3);
        planta := upper($4);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = ANY (uso)
	AND coddpto = adscripcion
        AND substring(codigo from 1 for 6) LIKE zona || '__' || planta
	AND codigo != '0000PB997';

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieestanciasocupadas(integer[], character varying, character varying, character varying) IS 'Obtiene la superficie de las estancias ocupadas y adscritas a un departamento SIGUANET de una lista de actividades SIGUANET en una planta de un campus/sede de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantazona_superficieestanciasocupadas(ARRAY[1,2,3,5], ''B101'', ''00'', ''PB'');
- select quest_plantazona_superficieestanciasocupadas(ARRAY[1,2,3,5], ''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_superficieestanciasocupadas(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
 superficie double precision;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT sum(st_area(t.geometria)) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo IN (SELECT codigo FROM todaspersonas) 
             AND substring(t.codigo from 1 for 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ';' INTO superficie;
 RETURN superficie;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_superficieestanciasocupadas(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Obtiene la superficie de las estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede.
SINTAXIS
SELECT quest_plantazona_superficieestanciasocupadas(''crue'',''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_superficieestanciasocupadasbec(integer, character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	zona varchar;
	planta varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        zona := upper($2);
        planta := upper($3);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM becarios)
	AND actividad = $1
	AND substring(codigo from 1 for 6) LIKE upper(zona || '__' || planta);
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieestanciasocupadasbec(integer, character varying, character varying) IS 'Obtiene la superficie de las estancias de un campus/sede y una planta ocupadas por becarios de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantazona_superficieestanciasocupadasbecarios(8, ''00'', ''PB'');
- SELECT quest_plantazona_superficieestanciasocupadasbecarios(8, ''00'', ''PB'');
';

CREATE FUNCTION quest_plantazona_superficieestanciasocupadasbec(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	zona varchar;
	planta varchar;
	superficie float8;
BEGIN
        uso := $1;
        zona := upper($2);
        planta := upper($3);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM becarios)
	AND actividad = ANY ($1)
	AND substring(codigo from 1 for 6) LIKE upper(zona || '__' || planta);

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieestanciasocupadasbec(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un campus/sede y una planta ocupadas por becarios de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantazona_superficieestanciasocupadasbecarios(ARRAY[8,9,16], ''00'',''P1'');
- SELECT quest_plantazona_superficieestanciasocupadasbecarios(ARRAY[8,9,16], ''00'',''P1'');
';

CREATE FUNCTION quest_plantazona_superficieestanciasocupadasbec(integer[], character varying, character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM becarios)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 1 for 6) LIKE upper($3 || '__' || $4)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieestanciasocupadasbec(integer[], character varying, character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por becarios de una lista de actividades SIGUANET y en una planta de un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_plantazona_superficieestanciasocupadasbec(ARRAY[1,2,3,5], ''B101'', ''00'', ''PB'');
- select quest_plantazona_superficieestanciasocupadasbec(ARRAY[1,2,3,5], ''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_superficieestanciasocupadasext(integer, character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	zona varchar;
	planta varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        zona := upper($2);
        planta := upper($3);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalexternos)
	AND actividad = $1
	AND substring(codigo from 1 for 6) LIKE upper(zona || '__' || planta);
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieestanciasocupadasext(integer, character varying, character varying) IS 'Obtiene la superficie de las estancias de un campus/sede y una planta ocupadas por externos de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantazona_superficieestanciasocupadasexternos(8, ''00'', ''PB'');
- SELECT quest_plantazona_superficieestanciasocupadasexternos(8, ''00'', ''PB'');
';

CREATE FUNCTION quest_plantazona_superficieestanciasocupadasext(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	zona varchar;
	planta varchar;
	superficie float8;
BEGIN
        uso := $1;
        zona := upper($2);
        planta := upper($3);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalexternos)
	AND actividad = ANY ($1)
	AND substring(codigo from 1 for 6) LIKE upper(zona || '__' || planta);

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieestanciasocupadasext(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un campus/sede y una planta ocupadas por externos de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantazona_superficieestanciasocupadasexternos(ARRAY[8,9,16], ''00'',''P1'');
- SELECT quest_plantazona_superficieestanciasocupadasexternos(ARRAY[8,9,16], ''00'',''P1'');
';

CREATE FUNCTION quest_plantazona_superficieestanciasocupadasext(integer[], character varying, character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalexternos)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 1 for 6) LIKE upper($3 || '__' || $4)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieestanciasocupadasext(integer[], character varying, character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por externos de una lista de actividades SIGUANET y en una planta de un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_plantazona_superficieestanciasocupadasext(ARRAY[1,2,3,5], ''B101'', ''00'', ''PB'');
- select quest_plantazona_superficieestanciasocupadasext(ARRAY[1,2,3,5], ''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_superficieestanciasocupadaspas(integer, character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	zona varchar;
	planta varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        zona := upper($2);
        planta := upper($3);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpas)
	AND actividad = $1
	AND substring(codigo from 1 for 6) LIKE upper(zona || '__' || planta);
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieestanciasocupadaspas(integer, character varying, character varying) IS 'Obtiene la superficie de las estancias de un campus/sede y una planta ocupadas por PAS de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantazona_superficieestanciasocupadaspas(8, ''00'', ''PB'');
- SELECT quest_plantazona_superficieestanciasocupadaspas(8, ''00'', ''PB'');
';

CREATE FUNCTION quest_plantazona_superficieestanciasocupadaspas(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	zona varchar;
	planta varchar;
	superficie float8;
BEGIN
        uso := $1;
        zona := upper($2);
        planta := upper($3);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpas)
	AND actividad = ANY ($1)
	AND substring(codigo from 1 for 6) LIKE upper(zona || '__' || planta);

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieestanciasocupadaspas(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un campus/sede y una planta ocupadas por PAS de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantazona_superficieestanciasocupadaspas(ARRAY[8,9,16], ''00'',''P1'');
- SELECT quest_plantazona_superficieestanciasocupadaspas(ARRAY[8,9,16], ''00'',''P1'');
';

CREATE FUNCTION quest_plantazona_superficieestanciasocupadaspas(integer[], character varying, character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpas)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 1 for 6) LIKE upper($3 || '__' || $4)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieestanciasocupadaspas(integer[], character varying, character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por PAS de una lista de actividades SIGUANET y en una planta de un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_plantazona_superficieestanciasocupadaspas(ARRAY[1,2,3,5], ''B101'', ''00'', ''PB'');
- select quest_plantazona_superficieestanciasocupadaspas(ARRAY[1,2,3,5], ''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_superficieestanciasocupadaspdi(integer, character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	zona varchar;
	planta varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        zona := upper($2);
        planta := upper($3);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpdi)
	AND actividad = $1
	AND substring(codigo from 1 for 6) LIKE upper(zona || '__' || planta);
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieestanciasocupadaspdi(integer, character varying, character varying) IS 'Obtiene la superficie de las estancias de un campus/sede y una planta ocupadas por PDI de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantazona_superficieestanciasocupadaspdi(8, ''00'', ''PB'');
- SELECT quest_plantazona_superficieestanciasocupadaspdi(8, ''00'', ''PB'');
';

CREATE FUNCTION quest_plantazona_superficieestanciasocupadaspdi(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	zona varchar;
	planta varchar;
	superficie float8;
BEGIN
        uso := $1;
        zona := upper($2);
        planta := upper($3);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpdi)
	AND actividad = ANY ($1)
	AND substring(codigo from 1 for 6) LIKE upper(zona || '__' || planta);

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieestanciasocupadaspdi(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un campus/sede y una planta ocupadas por PDI de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_plantazona_superficieestanciasocupadaspdi(ARRAY[8,9,16], ''00'',''P1'');
- SELECT quest_plantazona_superficieestanciasocupadaspdi(ARRAY[8,9,16], ''00'',''P1'');
';

CREATE FUNCTION quest_plantazona_superficieestanciasocupadaspdi(integer[], character varying, character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpdi)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 1 for 6) LIKE upper($3 || '__' || $4)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieestanciasocupadaspdi(integer[], character varying, character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por PDI de una lista de actividades SIGUANET y en una planta de un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_plantazona_superficieestanciasocupadaspdi(ARRAY[1,2,3,5], ''B101'', ''00'', ''PB'');
- select quest_plantazona_superficieestanciasocupadaspdi(ARRAY[1,2,3,5], ''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_superficieutil(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE a.util = true AND substring(t.codigo from 1 for 6) LIKE upper($1 || '__' || $2);
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieutil(character varying, character varying) IS 'Obtiene la superficie útil de un campus/sede y una planta de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantazona_superficieutil(''00'', ''P1'');
- select quest_plantazona_superficieutil(''00'', ''P1'');
';

CREATE FUNCTION quest_plantazona_superficieutil(character varying, character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) 
	FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE a.util = true 
	AND t.coddpto = upper($1)
	AND substring(t.codigo from 1 for 6) LIKE upper($2 || '__' || $3);
$_$;

COMMENT ON FUNCTION quest_plantazona_superficieutil(character varying, character varying, character varying) IS 'Obtiene la superficie útil de un departamento SIGUANET en una planta de un campus/sede de la Universidad.
Se ejecuta de dos formas:
- select * from quest_plantazona_superficieutil(''B101'', ''00'', ''PB'');
- select quest_plantazona_superficieutil(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_ubicaciones(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE codigo LIKE $1 || '__' || $2 || '___';
$_$;

COMMENT ON FUNCTION quest_plantazona_ubicaciones(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de una planta de un campus/sede de la Universidad.
La claúsula WHERE de la consulta no sigue el patrón del resto de funciones pero en este caso es la única que ofrece rendimiento aceptable con la vista quest_ubicaciones.
SINTAXIS
- SELECT * FROM quest_zona_ubicaciones(''00'', ''PB'')';

CREATE FUNCTION quest_plantazona_ubicaciones(character varying, character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT DISTINCT v.* FROM quest_ubicaciones v
	JOIN quest_personas2 p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE p.cod_depto = upper($1)
	AND v.codzona = upper($2)
	AND v.enumplanta = upper($3)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantazona_ubicaciones(character varying, character varying, character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para las personas ubicadas en estancias adscritas a un determinado departamento o unidad en una planta de un campus o sede.
SINTAXIS:
- SELECT * FROM quest_plantazona_ubicaciones(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_ubicaciones(integer, character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones WHERE actividad = $1 AND substring(codigo from 1 for 2) = $2 AND substring(codigo from 5 for 2) = $3;
$_$;

COMMENT ON FUNCTION quest_plantazona_ubicaciones(integer, character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en una planta de un campus o sede.
SINTAXIS
- SELECT * FROM quest_plantazona_ubicaciones(7, ''00'', ''PB'')';

CREATE FUNCTION quest_plantazona_ubicaciones(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND substring(codigo FROM 1 FOR 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_ubicaciones(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para las personas ubicadas en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad, en una planta de un campus o sede.
SINTAXIS:
- SELECT * FROM quest_plantazona_ubicaciones(''crue'', ''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_ubicacionesbecarios(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones
   WHERE esbecario = true 
   AND locbecario = true
   AND reftbl = 'becarios'
   AND (actividad <= 50 OR actividad = 99)
   AND codigo LIKE $1 || '__' || $2 || '___';
$_$;

COMMENT ON FUNCTION quest_plantazona_ubicacionesbecarios(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de un campus/sede de la Universidad.
La claúsula WHERE de la consulta no sigue el patrón del resto de funciones pero en este caso es la que ofrece mejor rendimiento con la vista quest_ubicaciones.
La condición where matiza que sólo devuelve estancias con BECARIOS
SINTAXIS
- SELECT * FROM quest_plantazona_ubicacionesbecarios(''00'', ''PB'')';

CREATE FUNCTION quest_plantazona_ubicacionesbecarios(character varying, character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_becarios p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.esbecario = true 
	AND v.locbecario = true
	AND v.reftbl = 'becarios'
	AND p.cod_depto_centro_subunidad = upper($1)
	AND v.codzona = upper($2)
	AND v.enumplanta = upper($3)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantazona_ubicacionesbecarios(character varying, character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociación becario/estancia de los becarios adscritos a un departamento SIGUANET ubicados en una planta de un campus/sede de la Universidad.
SINTAXIS
- select * from quest_plantazona_ubicacionesbecarios(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_ubicacionesbecarios(integer, character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 
   AND substring(codigo from 1 for 2) = $2 
   AND substring(codigo from 5 for 2) = $3 
   AND esbecario = true 
   AND locbecario = true
   AND reftbl = 'becarios';
$_$;

COMMENT ON FUNCTION quest_plantazona_ubicacionesbecarios(integer, character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion becario/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en una planta de un campus o sede.
SINTAXIS
- SELECT * FROM quest_plantazona_ubicacionesbecarios(7, ''00'', ''P1'')';

CREATE FUNCTION quest_plantazona_ubicacionesbecarios(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND esbecario = true 
                       AND locbecario = true
                       AND reftbl = ''becarios''
                       AND substring(codigo FROM 1 FOR 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_ubicacionesbecarios(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los BECARIOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede.
SINTAXIS:
- SELECT * FROM quest_plantazona_ubicacionesbecarios(''crue'', ''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_ubicacionescargos(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE espdicargo = true 
   AND locpdicargo = true
   AND reftbl = 'personalpdi_cargos'
   AND (actividad <= 50 OR actividad = 99)
   AND codigo LIKE $1 || '__' || $2 || '___';
$_$;

COMMENT ON FUNCTION quest_plantazona_ubicacionescargos(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de un campus/sede de la Universidad.
La claúsula WHERE de la consulta no sigue el patrón del resto de funciones pero en este caso es la que ofrece mejor rendimiento con la vista quest_ubicaciones.
La condición where matiza que sólo devuelve estancias con CARGOS
SINTAXIS
- SELECT * FROM quest_plantazona_ubicacionescargos(''00'', ''PB'')';

CREATE FUNCTION quest_plantazona_ubicacionescargos(character varying, character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalpdi_cargos p ON v.nif = p.nif AND v.codigo = p.codigo
	JOIN cargos_dpto cd ON p.cod_cargo = cd.cod_cargo
	WHERE v.espdicargo = true 
	AND v.locpdicargo = true
	AND v.reftbl = 'personalpdi_cargos'
	AND cd.coddpto = upper($1)
	AND v.codzona = upper($2)
	AND v.enumplanta = upper($3)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantazona_ubicacionescargos(character varying, character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociación pdicargo/estancia de los cargos adscritos a un departamento SIGUANET ubicados en una planta de un campus/sede de la Universidad.
SINTAXIS
- select * from quest_plantazona_ubicacionescargos(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_ubicacionesexternos(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones
   WHERE esexterno = true
   AND locexterno = true
   AND reftbl = 'personalexternos'   
   AND (actividad <= 50 OR actividad = 99)
   AND codigo LIKE $1 || '__' || $2 || '___';
$_$;

COMMENT ON FUNCTION quest_plantazona_ubicacionesexternos(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de un campus/sede de la Universidad.
La claúsula WHERE de la consulta no sigue el patrón del resto de funciones pero en este caso es la que ofrece mejor rendimiento con la vista quest_ubicaciones.
La condición where matiza que sólo devuelve estancias DE EXTERNOS
SINTAXIS
- SELECT * FROM quest_plantazona_ubicacionesexternos(''00'', ''PB'')';

CREATE FUNCTION quest_plantazona_ubicacionesexternos(character varying, character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalexternos p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.esexterno = true 
	AND v.locexterno = true
	AND v.reftbl = 'personalexternos'
	AND p.cod_dpto_sigua = upper($1)
	AND v.codzona = upper($2)
	AND v.enumplanta = upper($3)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantazona_ubicacionesexternos(character varying, character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion externo/estancia de los externos adscritos a un departamento SIGUANET ubicados en una planta de un campus/sede de la Universidad.
SINTAXIS
- select * from quest_plantazona_ubicacionesexternos(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_ubicacionesexternos(integer, character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 
   AND substring(codigo from 1 for 2) = $2 
   AND substring(codigo from 5 for 2) = $3 
   AND esexterno = true 
   AND locexterno = true
   AND reftbl = 'externos';
$_$;

COMMENT ON FUNCTION quest_plantazona_ubicacionesexternos(integer, character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion externo/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en una planta de un campus o sede.
SINTAXIS
- SELECT * FROM quest_plantazona_ubicacionesexternos(7, ''00'', ''P1'')';

CREATE FUNCTION quest_plantazona_ubicacionesexternos(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND esexterno = true 
                       AND locexterno = true
                       AND reftbl = ''personalexternos''
                       AND substring(codigo FROM 1 FOR 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_ubicacionesexternos(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los empleados EXTERNOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede.
SINTAXIS:
- SELECT * FROM quest_plantazona_ubicacionesexternos(''crue'', ''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_ubicacionespas(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_ubicaciones 
	WHERE espas = true
	AND locpas = true
	AND reftbl = 'personalpas'
	AND (actividad <= 50 OR actividad = 99)
	AND codigo LIKE $1 || '__' || $2 || '___';
$_$;

COMMENT ON FUNCTION quest_plantazona_ubicacionespas(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de una planta de un campus/sede de la Universidad.
La claúsula WHERE de la consulta no sigue el patrón del resto de funciones pero en este caso es la que ofrece mejor rendimiento con la vista quest_ubicaciones.
La condición where matiza que sólo devuelve estancias con pas.
SINTAXIS
- SELECT * FROM quest_plantazona_ubicacionespas(''00'', ''PB'')';

CREATE FUNCTION quest_plantazona_ubicacionespas(character varying, character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalpas p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.espas = true 
	AND v.locpas = true
	AND v.reftbl = 'personalpas'
	AND p.cod_unidad = upper($1)
	AND v.codzona = upper($2)
	AND v.enumplanta = upper($3)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantazona_ubicacionespas(character varying, character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion pas/estancia de los pas adscritos a un departamento SIGUANET ubicados en una planta de un campus/sede de la Universidad.
La condición where matiza que sólo devuelve estancias con pas
SINTAXIS
- select * from quest_plantazona_ubicacionespas(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_ubicacionespas(integer, character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 
   AND substring(codigo from 1 for 2) = $2 
   AND substring(codigo from 5 for 2) = $3 
   AND espas = true 
   AND locpas = true
   AND reftbl = 'personalpas';
$_$;

COMMENT ON FUNCTION quest_plantazona_ubicacionespas(integer, character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion PAS/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en una planta de un campus o sede.
SINTAXIS
- SELECT * FROM quest_plantazona_ubicacionespas(7, ''00'', ''P1'')';

CREATE FUNCTION quest_plantazona_ubicacionespas(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND espas = true 
                       AND locpas = true
                       AND reftbl = ''personalpas''
                       AND substring(codigo FROM 1 FOR 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_ubicacionespas(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los PAS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede.
SINTAXIS:
- SELECT * FROM quest_plantazona_ubicacionespas(''crue'', ''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_ubicacionespdi(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE espdi = true 
   AND locpdi = true
   AND reftbl = 'personalpdi'
   AND (actividad <= 50 OR actividad = 99)
   AND codigo LIKE $1 || '__' || $2 || '___';
$_$;

COMMENT ON FUNCTION quest_plantazona_ubicacionespdi(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de una planta de un campus/sede de la Universidad.
La claúsula WHERE de la consulta no sigue el patrón del resto de funciones pero en este caso es la que ofrece mejor rendimiento con la vista quest_ubicaciones.
La condición where matiza que sólo devuelve estancias con PDI
SINTAXIS
- SELECT * FROM quest_plantazona_ubicacionespdi(''00'', ''PB'')';

CREATE FUNCTION quest_plantazona_ubicacionespdi(character varying, character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalpdi p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.espdi = true 
	AND v.locpdi = true
	AND v.reftbl = 'personalpdi'
	AND p.cod_depto = upper($1)
	AND v.codzona = upper($2)
	AND v.enumplanta = upper($3)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_plantazona_ubicacionespdi(character varying, character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociación pdi/estancia de los pdi adscritos a un departamento SIGUANET ubicados en una planta de un campus/sede de la Universidad.
SINTAXIS
- select * from quest_plantazona_ubicacionespdi(''B101'', ''00'', ''PB'');';

CREATE FUNCTION quest_plantazona_ubicacionespdi(integer, character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 
   AND substring(codigo from 1 for 2) = $2 
   AND substring(codigo from 5 for 2) = $3 
   AND espdi = true 
   AND locpdi = true
   AND reftbl = 'personalpdi';
$_$;

COMMENT ON FUNCTION quest_plantazona_ubicacionespdi(integer, character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion pdi/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en una planta de un campus o sede.
SINTAXIS
- SELECT * FROM quest_plantazona_ubicacionespdi(7, ''00'', ''P1'')';

CREATE FUNCTION quest_plantazona_ubicacionespdi(tipo character varying, denominacion character varying, zona character varying, planta character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND espdi = true 
                       AND locpdi = true
                       AND reftbl = ''personalpdi''
                       AND substring(codigo FROM 1 FOR 6) LIKE ' || quote_literal(upper(zona || '__' || planta)) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_plantazona_ubicacionespdi(tipo character varying, denominacion character varying, zona character varying, planta character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los PDI ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en una planta de un campus o sede.
SINTAXIS:
- SELECT * FROM quest_plantazona_ubicacionespdi(''crue'', ''DOCENCIA'', ''00'', ''PB'');';

CREATE FUNCTION quest_ua_densidad() RETURNS double precision
    LANGUAGE sql
    AS $$
	SELECT quest_ua_superficieestanciasocupadas(ARRAY[4,5,7,8,9,16]) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE actividad = ANY (ARRAY[4,5,7,8,9,16])));
$$;

COMMENT ON FUNCTION quest_ua_densidad() IS 'Obtiene los m2 de espacio de trabajo por empleado en la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_ua_densidad(''B101'');
- select quest_ua_densidad(''B101'');';

CREATE FUNCTION quest_ua_densidad(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_ua_superficieestanciasocupadas(ARRAY[4,5,7,8,9,16], $1) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE actividad = ANY (ARRAY[4,5,7,8,9,16])
										       AND coddpto = upper($1)));
$_$;

COMMENT ON FUNCTION quest_ua_densidad(character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado de un departamento SIGUANET. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_ua_densidad(''B101'');
- select quest_ua_densidad(''B101'');';

CREATE FUNCTION quest_ua_densidad(integer) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_ua_superficieestanciasocupadas($1) INTO superficie; 
 SELECT count(nif) INTO poblacion FROM todaspersonas WHERE codigo IN (SELECT codigo FROM todasestancias WHERE actividad = $1);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_ua_densidad(integer) IS 'Obtiene los m2 de espacio de trabajo por empleado en las estancias de una determinada actividad SIGUANET para toda la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_ua_densidad(4);
- select quest_ua_densidad(8);';

CREATE FUNCTION quest_ua_densidad(tipo character varying, denominacion character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  SELECT quest_ua_superficieestanciasocupadas(tipo, denominacion) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM todaspersonas WHERE codigo IN
            (SELECT codigo FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;   
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_densidad(tipo character varying, denominacion character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_ua_densidad(''crue'', ''DOCENCIA'');';

CREATE FUNCTION quest_ua_densidadbecarios() RETURNS double precision
    LANGUAGE sql
    AS $$
	SELECT quest_ua_superficieestanciasocupadasbec(ARRAY[4,5,7]) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM becarios)
										       AND actividad = ANY (ARRAY[4,5,7])));
$$;

COMMENT ON FUNCTION quest_ua_densidadbecarios() IS 'Obtiene los m2 de espacio de trabajo por becario de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_ua_densidadbecarios();
- select quest_ua_densidadbecarios();
';

CREATE FUNCTION quest_ua_densidadbecarios(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_ua_superficieestanciasocupadasbec(ARRAY[4,5,7], $1) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM becarios)
										       AND actividad = ANY (ARRAY[4,5,7])
										       AND coddpto = upper($1)));
$_$;

COMMENT ON FUNCTION quest_ua_densidadbecarios(character varying) IS 'Obtiene los m2 de espacio de trabajo por becario en un departamento SIGUANET. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_ua_densidadbecarios(''B101'');
- select quest_ua_densidadbecarios(''B101'');';

CREATE FUNCTION quest_ua_densidadbecarios(integer) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_ua_superficieestanciasocupadasbec($1) INTO superficie; 
 SELECT count(nif) INTO poblacion FROM becarios WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM becarios)
                                           AND actividad = $1);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_ua_densidadbecarios(integer) IS 'Obtiene los m2 de espacio de trabajo por becario en las estancias de una determinada actividad SIGUANET para toda la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_ua_densidadbecarios(4);
- select quest_ua_densidadbecarios(8);';

CREATE FUNCTION quest_ua_densidadbecarios(tipo character varying, denominacion character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_ua_superficieestanciasocupadasbec(actlist) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM becarios WHERE codigo IN
            (SELECT codigo FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_densidadbecarios(tipo character varying, denominacion character varying) IS 'Obtiene los m2 de espacio de trabajo por BECARIO en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_ua_densidadbecarios(''crue'', ''DOCENCIA'');';

CREATE FUNCTION quest_ua_densidadexternos() RETURNS double precision
    LANGUAGE sql
    AS $$
	SELECT quest_ua_superficieestanciasocupadasext(ARRAY[7,8]) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalexternos)
										       AND actividad = ANY (ARRAY[7,8])));
$$;

COMMENT ON FUNCTION quest_ua_densidadexternos() IS 'Obtiene los m2 de espacio de trabajo por externo de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_ua_densidadexternos();
- select quest_ua_densidadexternos();
';

CREATE FUNCTION quest_ua_densidadexternos(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_ua_superficieestanciasocupadasext(ARRAY[7,8], $1) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalexternos)
										       AND actividad = ANY (ARRAY[7,8])
										       AND coddpto = upper($1)));
$_$;

COMMENT ON FUNCTION quest_ua_densidadexternos(character varying) IS 'Obtiene los m2 de espacio de trabajo por externo en un departamento SIGUANET. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_ua_densidadexternos(''B101'');
- select quest_ua_densidadexternos(''B101'');';

CREATE FUNCTION quest_ua_densidadexternos(integer) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_ua_superficieestanciasocupadasext($1) INTO superficie; 
 SELECT count(nif) INTO poblacion FROM personalexternos WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalexternos)
                                           AND actividad = $1);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_ua_densidadexternos(integer) IS 'Obtiene los m2 de espacio de trabajo por externo en las estancias de una determinada actividad SIGUANET para toda la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_ua_densidadexternos(4);
- select quest_ua_densidadexternos(8);';

CREATE FUNCTION quest_ua_densidadexternos(tipo character varying, denominacion character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_ua_superficieestanciasocupadasext(actlist) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM personalexternos WHERE codigo IN
            (SELECT codigo FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_densidadexternos(tipo character varying, denominacion character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado EXTERNO en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_ua_densidadexternos(''crue'', ''DOCENCIA'');';

CREATE FUNCTION quest_ua_densidadpas() RETURNS double precision
    LANGUAGE sql
    AS $$
	SELECT quest_ua_superficieestanciasocupadaspas(ARRAY[4,5,8,9,16]) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpas)
										       AND actividad = ANY (ARRAY[4,5,8,9,16])));
$$;

COMMENT ON FUNCTION quest_ua_densidadpas() IS 'Obtiene los m2 de espacio de trabajo por PAS de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_ua_densidadpas();
- select quest_ua_densidadpas();
';

CREATE FUNCTION quest_ua_densidadpas(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_ua_superficieestanciasocupadaspas(ARRAY[4,5,8,9,16], $1) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpas)
										       AND actividad = ANY (ARRAY[4,5,8,9,16])
										       AND coddpto = upper($1)));
$_$;

COMMENT ON FUNCTION quest_ua_densidadpas(character varying) IS 'Obtiene los m2 de espacio de trabajo por PAS en un departamento SIGUANET. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_ua_densidadpas(''B101'');
- select quest_ua_densidadpas(''B101'');';

CREATE FUNCTION quest_ua_densidadpas(integer) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_ua_superficieestanciasocupadaspas($1) INTO superficie; 
 SELECT count(nif) INTO poblacion FROM personalpas WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpas)
                                           AND actividad = $1);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_ua_densidadpas(integer) IS 'Obtiene los m2 de espacio de trabajo por pas en las estancias de una determinada actividad SIGUANET para toda la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_ua_densidadpas(4);
- select quest_ua_densidadpas(8);';

CREATE FUNCTION quest_ua_densidadpas(tipo character varying, denominacion character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_ua_superficieestanciasocupadaspas(actlist) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM personalpas WHERE codigo IN
            (SELECT codigo FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_densidadpas(tipo character varying, denominacion character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado tipo PAS en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_ua_densidadpas(''crue'', ''DOCENCIA'');';

CREATE FUNCTION quest_ua_densidadpdi() RETURNS double precision
    LANGUAGE sql
    AS $$
	SELECT quest_ua_superficieestanciasocupadaspdi(ARRAY[4,5,7]) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpdi)
										       AND actividad = ANY (ARRAY[4,5,7])));
$$;

COMMENT ON FUNCTION quest_ua_densidadpdi() IS 'Obtiene los m2 de espacio de trabajo por PDI de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_ua_densidadpdi();
- select quest_ua_densidadpdi();
';

CREATE FUNCTION quest_ua_densidadpdi(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_ua_superficieestanciasocupadaspdi(ARRAY[4,5,7], $1) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpdi)
										       AND actividad = ANY (ARRAY[4,5,7])
										       AND coddpto = upper($1)));
$_$;

COMMENT ON FUNCTION quest_ua_densidadpdi(character varying) IS 'Obtiene los m2 de espacio de trabajo por PDI en un departamento SIGUANET. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_ua_densidadpdi(''B101'');
- select quest_ua_densidadpdi(''B101'');';

CREATE FUNCTION quest_ua_densidadpdi(integer) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_ua_superficieestanciasocupadaspdi($1) INTO superficie; 
 SELECT count(nif) INTO poblacion FROM personalpdi WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpdi)
                                           AND actividad = $1);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_ua_densidadpdi(integer) IS 'Obtiene los m2 de espacio de trabajo por pdi en las estancias de una determinada actividad SIGUANET para toda la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_ua_densidadpdi(4);
- select quest_ua_densidadpdi(8);';

CREATE FUNCTION quest_ua_densidadpdi(tipo character varying, denominacion character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_ua_superficieestanciasocupadaspdi(actlist) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM personalpdi WHERE codigo IN
            (SELECT codigo FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_densidadpdi(tipo character varying, denominacion character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado tipo PDI en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_ua_densidadpdi(''crue'', ''DOCENCIA'');';

CREATE FUNCTION quest_ua_numdespachosnoocupados() RETURNS bigint
    LANGUAGE sql
    AS $$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7;
$$;

CREATE FUNCTION quest_ua_numdespachosocupados() RETURNS bigint
    LANGUAGE sql
    AS $$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7;
$$;

CREATE FUNCTION quest_ua_numestancias() RETURNS bigint
    LANGUAGE sql
    AS $$
	SELECT count(codigo) FROM 
	(SELECT codigo from todasestancias group by codigo) AS foo;
$$;

COMMENT ON FUNCTION quest_ua_numestancias() IS 'Obtiene el nº de estancias de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_ua_numestancias();
- select quest_ua_numestancias();
';

CREATE FUNCTION quest_ua_superficie() RETURNS double precision
    LANGUAGE sql
    AS $$
SELECT sum(st_area(geometria)) FROM todasestancias WHERE actividad != 120;
$$;

COMMENT ON FUNCTION quest_ua_superficie() IS 'Obtiene la superficie total de la Universidad.';

CREATE FUNCTION quest_ua_superficiedespachosnoocupados() RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
    sumag float8;
BEGIN

   SELECT sum(st_area(t.geometria)) INTO sumag
   FROM todasestancias t
   WHERE t.codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND t.actividad = 7;

	RETURN sumag;

END;
$$;

CREATE FUNCTION quest_ua_superficiedespachosocupados() RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
    sumag float8;
BEGIN

   SELECT sum(st_area(t.geometria)) INTO sumag
   FROM todasestancias t
   WHERE t.codigo IN (SELECT codigo FROM todaspersonas)
   AND t.actividad = 7;

	RETURN sumag;

END;
$$;

CREATE VIEW quest_sumario_estancias AS
    SELECT quest_ua_numestancias() AS numest, quest_ua_superficie() AS sumag, quest_ua_numdespachosnoocupados() AS desp_desocupados, quest_ua_numdespachosocupados() AS desp_ocupados, quest_ua_superficiedespachosnoocupados() AS sup_desp_desocupados, quest_ua_superficiedespachosocupados() AS sup_desp_ocupados;

CREATE FUNCTION quest_ua_estadisticaestancias() RETURNS SETOF quest_sumario_estancias
    LANGUAGE sql
    AS $$

SELECT * FROM quest_sumario_estancias;

$$;

COMMENT ON FUNCTION quest_ua_estadisticaestancias() IS 'Esta función utiliza una vista llamada
quest_sumario_estancias, que a su vez llama a varias funciones ya creadas para generar un resumen
estadístico de estancias de toda la Universidad.
SINTAXIS
- SELECT quest_ua_estadisticaestancias()';

CREATE FUNCTION quest_ua_estadisticaestancias(character varying) RETURNS SETOF record
    LANGUAGE plpgsql
    AS $_$
DECLARE
   adscripcion varchar;
   micursor refcursor;

  fila record;
BEGIN
   adscripcion := upper($1);

  OPEN micursor FOR
   SELECT quest_ua_numestancias(adscripcion) AS numest, 
   quest_ua_superficie(adscripcion) AS sumag, 
   quest_ua_numdespachosnoocupados(adscripcion) AS desp_desocupados, 
   quest_ua_numdespachosocupados(adscripcion) AS desp_ocupados, 
   quest_ua_superficiedespachosnoocupados(adscripcion) AS sup_desp_desocupados, 
   quest_ua_superficiedespachosocupados(adscripcion) AS sup_desp_ocupados;


  FETCH micursor INTO fila;
  WHILE FOUND LOOP
    RETURN NEXT fila;
    FETCH micursor INTO fila;
  END LOOP;
  RETURN;

END
$_$;

COMMENT ON FUNCTION quest_ua_estadisticaestancias(character varying) IS 'Obtiene estadísticas de un determinado dpto sigua.
SINTAXIS:
SELECT * from quest_ua_estadisticaestancias(''B107'')  
AS (numest int8, sumag float8, desp_desocupados int8, desp_ocupados int8, sup_desp_desocupados float8, sup_desp_ocupados float8);
';

CREATE FUNCTION quest_ua_estadisticaestancias(integer) RETURNS SETOF record
    LANGUAGE plpgsql
    AS $_$
DECLARE
   micursor refcursor;
   fila record;
BEGIN

 IF $1 NOT IN (SELECT codactividad FROM actividades) THEN
    RAISE exception 'No existe un código de actividad con ese valor (%) en la tabla actividades .', $1;
 END IF; 

  OPEN micursor FOR

   SELECT quest_ua_numestancias($1) as numest,
   quest_ua_superficie ($1) as sumag;

  FETCH micursor INTO fila;
  WHILE FOUND LOOP
    RETURN NEXT fila;
    FETCH micursor INTO fila;
  END LOOP;
  RETURN;

END
$_$;

COMMENT ON FUNCTION quest_ua_estadisticaestancias(integer) IS 'Obtiene estadísticas a nivel de actividad sigua. 
SINTAXIS:
SELECT * from quest_ua_estadisticaestancias(7)  
AS (numest int8, sumag float8);
';

CREATE FUNCTION quest_ua_estadisticaestancias(character varying, character varying) RETURNS SETOF record
    LANGUAGE plpgsql
    AS $_$
DECLARE
   micursor refcursor;
   tipo varchar;
   denoactividad varchar;
   fila record;
   control bool;
BEGIN
   tipo := lower($1);

-- Control de tipo
   IF tipo = 'crue' OR	tipo = 'u21' THEN
	denoactividad := upper($2);
   ELSIF tipo = 'activresum' THEN
	denoactividad := $2;
   ELSE
	RAISE EXCEPTION 'El grupo de actividad no es crue, ni u21 ni activresum. Se desconoce %', tipo;
   END IF;
-- apertura del cursor
  OPEN micursor FOR

   SELECT quest_ua_numestancias(tipo, denoactividad) as numest,
   quest_ua_superficie (tipo, denoactividad) as sumag;

  FETCH micursor INTO fila;
  WHILE FOUND LOOP
    RETURN NEXT fila;
    FETCH micursor INTO fila;
  END LOOP;
  RETURN;
END
$_$;

CREATE FUNCTION quest_ua_estadisticaestancias(integer, character varying) RETURNS SETOF record
    LANGUAGE plpgsql
    AS $_$
DECLARE
   micursor refcursor;
   adscripcion varchar;
   fila record;

BEGIN
   adscripcion := upper($2);


-- apertura del cursor
  OPEN micursor FOR

   SELECT quest_ua_numestancias($1, adscripcion) as numest,
   quest_ua_superficie ($1, adscripcion) as sumag;

  FETCH micursor INTO fila;
  WHILE FOUND LOOP
    RETURN NEXT fila;
    FETCH micursor INTO fila;
  END LOOP;
  RETURN;



END
$_$;

COMMENT ON FUNCTION quest_ua_estadisticaestancias(integer, character varying) IS 'Obtiene las estadísticas de un dpto sigua y una actividad sigua.
SINTAXIS:
SELECT * FROM quest_ua_estadisticaestancias(5, ''B101'') AS (numest int8, sumag float8);
';

CREATE FUNCTION quest_ua_estadisticaestancias(character varying, character varying, character varying) RETURNS SETOF record
    LANGUAGE plpgsql
    AS $_$
DECLARE
   adscripcion varchar;
   micursor refcursor;
   agrupa varchar;
   denoactividad varchar;
   fila record;
   control bool;
BEGIN
   agrupa := lower($1);
   adscripcion := upper($3);

-- Control de tipo
   IF agrupa = 'crue' OR agrupa = 'u21' THEN
	denoactividad := upper($2);
   ELSEIF agrupa = 'actviresum' THEN
	denoactividad := $2;
   ELSE
	RAISE EXCEPTION 'El grupo de actividad no es crue, ni u21 ni activresum. Se desconoce %', agrupa;
   END IF; 

-- apertura del cursor
  OPEN micursor FOR

   SELECT quest_ua_numestancias(agrupa, denoactividad, adscripcion) as numest,
   quest_ua_superficie (agrupa, denoactividad, adscripcion) as sumag;

  FETCH micursor INTO fila;
  WHILE FOUND LOOP
    RETURN NEXT fila;
    FETCH micursor INTO fila;
  END LOOP;
  RETURN;



END
$_$;

CREATE FUNCTION quest_ua_estanciasutiles() RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $$
SELECT * FROM quest_estancias
WHERE codigo IN (SELECT codigo FROM todaspersonas)
AND actividad = ANY(ARRAY[7,8]);
$$;

COMMENT ON FUNCTION quest_ua_estanciasutiles() IS 'Obtiene un dataset con todas las estancias utiles de la UA.
SINTAXIS:
- SELECT * FROM quest_ua_estanciasutiles();';

CREATE FUNCTION quest_ua_numadmonnoocupados() RETURNS bigint
    LANGUAGE sql
    AS $$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8;
$$;

CREATE FUNCTION quest_ua_numadmonnoocupados(character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    cuenta int8;
    adscripcion varchar;	
BEGIN
   adscripcion := upper($1);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_ua_numadmonnoocupados(character varying) IS 'Obtiene el nº de administraciones vacias de un departamento sigua. 
SINTAXIS:
- select * from quest_ua_numadmonnoocupados(''B101'');
- select quest_ua_numadmonnoocupados(''B101'');';

CREATE FUNCTION quest_ua_numadmonocupados() RETURNS bigint
    LANGUAGE sql
    AS $$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8;
$$;

CREATE FUNCTION quest_ua_numadmonocupados(character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    cuenta int8;
    adscripcion varchar;	
BEGIN
   adscripcion := upper($1);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_ua_numadmonocupados(character varying) IS 'Obtiene el nº de administraciones ocupadas de un departamento sigua. 
SINTAXIS:
- select * from quest_ua_numadmonocupados(''B101'');
- select quest_ua_numadmonocupados(''B101'');';

CREATE FUNCTION quest_ua_numbecarios() RETURNS bigint
    LANGUAGE sql
    AS $$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM becarios) aux;
$$;

COMMENT ON FUNCTION quest_ua_numbecarios() IS 'Obtiene el nº de becarios de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_ua_numbecarios();
- select quest_ua_numbecarios();
';

CREATE FUNCTION quest_ua_numbecarios(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM becarios
	WHERE cod_depto_centro_subunidad = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_numbecarios(character varying) IS 'Obtiene el total de becarios de un departamento SIGUANET. 
Se ejecuta de dos formas:
- select * from quest_ua_numbecarios(''B101'');
- select quest_ua_numbecarios(''B101'');';

CREATE FUNCTION quest_ua_numbecarios(integer) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM becarios WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_ua_numbecarios(tipo character varying, denominacion character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM becarios WHERE codigo IN
            (SELECT codigo FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_numbecarios(tipo character varying, denominacion character varying) IS 'Obtiene el nº de BECARIOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS:
- SELECT quest_ua_numbecarios(''crue'', ''DOCENCIA'');';

CREATE FUNCTION quest_ua_numbecariosnoubicados() RETURNS bigint
    LANGUAGE sql
    AS $$
   SELECT count(*) FROM quest_checklist_personas WHERE esbecario = true AND locbecario = false;
$$;

COMMENT ON FUNCTION quest_ua_numbecariosnoubicados() IS 'Obtiene el nº de becarios con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_numbecariosnoubicados();
- select quest_ua_numbecariosnoubicados();
';

CREATE FUNCTION quest_ua_numbecariosnoubicados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT v.nif) FROM quest_checklist_personas v, quest_personas2 tp
	WHERE tp.nif = v.nif
	AND v.esbecario = true
	AND v.locbecario = false
	AND tp.codigo = '0000PB997' 
	AND tp.cod_depto = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_numbecariosnoubicados(character varying) IS 'Obtiene el nº de becarios de un departamento SIGUANET con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_numbecariosnoubicados(''B101'');
- select quest_ua_numbecariosnoubicados(''B101'');';

CREATE FUNCTION quest_ua_numdespachosnoocupados(character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    cuenta int8;
    adscripcion varchar;	
BEGIN
   adscripcion := upper($1);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_ua_numdespachosnoocupados(character varying) IS 'Obtiene el nº de despachos vacios de un departamento sigua. 
SINTAXIS:
- select * from quest_ua_numdespachosnoocupados(''B101'');
- select quest_ua_numdespachosnoocupados(''B101'');';

CREATE FUNCTION quest_ua_numdespachosocupados(character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    cuenta int8;
    adscripcion varchar;	
BEGIN
   adscripcion := upper($1);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_ua_numdespachosocupados(character varying) IS 'Obtiene el nº de despachos ocupados de un departamento sigua. 
SINTAXIS:
- select * from quest_ua_numdespachosocupados(''B101'');
- select quest_ua_numdespachosocupados(''B101'');';

CREATE FUNCTION quest_ua_numestancias(integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    cuenta int8;

BEGIN
   IF $1 NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', $1;
   ELSE
	SELECT count(codigo) INTO cuenta FROM 
       (SELECT codigo from todasestancias WHERE actividad = $1 GROUP BY codigo) AS foo;
      RETURN cuenta;
   END IF;
   
END
$_$;

COMMENT ON FUNCTION quest_ua_numestancias(integer) IS 'Obtiene el nº de estancias de una determinada actividad sigua de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_ua_numestancias(7);
- select quest_ua_numestancias(8);';

CREATE FUNCTION quest_ua_numestancias(character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$DECLARE
    cuenta int8;
    adscripcion varchar;	
BEGIN
	adscripcion := upper($1);
	IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
	  RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
	END IF;

	SELECT count(codigo) INTO cuenta FROM 
	(SELECT t.codigo FROM todasestancias t, departamentossigua d 
	WHERE t.coddpto = d.cod_dpto_sigua
	AND t.coddpto = adscripcion 
	GROUP BY t.codigo) AS foo;

	RETURN cuenta;

END;
$_$;

COMMENT ON FUNCTION quest_ua_numestancias(character varying) IS 'Obtiene el total de estancias de un departamento sigua
SINTAXIS:
- SELECT quest_ua_numestancias(''B101'');';

CREATE FUNCTION quest_ua_numestancias(tipo character varying, denominacion character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
 cuenta bigint;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT count(codigo)  FROM 
  (SELECT t.codigo FROM todasestancias t, actividades a 
   WHERE t.actividad = a.codactividad
   AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ' GROUP BY t.codigo) AS foo' INTO cuenta;
 RETURN cuenta;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_numestancias(tipo character varying, denominacion character varying) IS 'Obtiene el nº de estancias de un tipo de grupo de actividad (crue, u21,activresum) y una denominación de grupo de actividad.
SINTAXIS
SELECT quest_ua_numestancias(''crue'',''DOCENCIA'');';

CREATE FUNCTION quest_ua_numestancias(integer, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
   adscripcion varchar;
   cuenta int8;
	
BEGIN
adscripcion := upper($2);
-- Control de errores
   IF $1 NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'El actividad % no está definida en la tabla Actividades.', $1;
   END IF;
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;


   SELECT count(DISTINCT codigo) INTO cuenta FROM 
   todasestancias
   WHERE actividad = $1
   AND coddpto = adscripcion;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_ua_numestancias(integer, character varying) IS 'Obtiene el nº de estancias de un departamento sigua y una actividad .
SINTAXIS:
- SELECT * FROM quest_ua_numestancias(7,''B101'');
- SELECT quest_ua_numestancias(7,''B101'');';

CREATE FUNCTION quest_ua_numestancias(character varying, character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    agrupa varchar;
    adscripcion varchar;
    cuenta int8;
	
BEGIN
adscripcion := upper($3);
agrupa := lower($1);

IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
   RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
END IF;

IF agrupa = 'crue' THEN
		SELECT count(codigo) INTO cuenta FROM 
		(SELECT t.codigo FROM todasestancias t, departamentossigua d, actividades a 
		WHERE t.coddpto = d.cod_dpto_sigua
		AND t.actividad = a.codactividad
		AND t.coddpto = adscripcion 
		AND a.crue = upper($2)
		GROUP BY t.codigo) AS foo;
		RETURN cuenta;
ELSEIF agrupa = 'u21' THEN
		SELECT count(codigo) INTO cuenta FROM 
		(SELECT t.codigo FROM todasestancias t, departamentossigua d, actividades a 
		WHERE t.coddpto = d.cod_dpto_sigua
		AND t.actividad = a.codactividad
		AND t.coddpto = adscripcion 
		AND a.u21 = upper($2)
		GROUP BY t.codigo) AS foo;
		RETURN cuenta;
ELSEIF agrupa = 'activresum' THEN
		SELECT count(codigo) INTO cuenta FROM 
		(SELECT t.codigo FROM todasestancias t, departamentossigua d, actividades a 
		WHERE t.coddpto = d.cod_dpto_sigua
		AND t.actividad = a.codactividad
		AND t.coddpto = adscripcion 
		AND a.activresum = $2
		GROUP BY t.codigo) AS foo;
		RETURN cuenta;
ELSE
     RAISE exception 'No existe un campo valido con ese nombre: (%). Debe de ser u21, activresum o crue ', $1;
END IF;
END;

$_$;

COMMENT ON FUNCTION quest_ua_numestancias(character varying, character varying, character varying) IS 'Obtiene el nº de estancias de un departamento sigua y una determinado valor de una actividad sigua.
SINTAXIS:
- SELECT * FROM quest_ua_numestancias(''crue'',''DOCENCIA'',''B101'');';

CREATE FUNCTION quest_ua_numestanciasdocentes() RETURNS bigint
    LANGUAGE sql
    AS $$
	SELECT count(codigo) FROM 
	(SELECT t.codigo from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE upper(a.activresum) = 'DOCENCIA'
         group by t.codigo) AS foo;
$$;

COMMENT ON FUNCTION quest_ua_numestanciasdocentes() IS 'Obtiene el nº de estancias de la Universidad cuya actividad pertenece al grupo "Docencia" de la tabla actividades. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_ua_numestanciasdocentes();
- select quest_ua_numestanciasdocentes();
';

CREATE FUNCTION quest_ua_numestanciasdocentes(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(codigo) FROM 
	(SELECT t.codigo from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE upper(a.activresum) = 'DOCENCIA'
	 AND t.coddpto = upper($1)
         group by t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_ua_numestanciasdocentes(character varying) IS 'Obtiene el nº total de estancias adscritas a un departamento SIGUANET cuya actividad pertenece al grupo "Docencia" de la tabla actividades. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_ua_numestanciasdocentes(''B101'');
- select quest_ua_numestanciasdocentes(''B101'');';

CREATE FUNCTION quest_ua_numestanciasnoocupadas(integer) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1;
$_$;

COMMENT ON FUNCTION quest_ua_numestanciasnoocupadas(integer) IS 'Obtiene el nº de estancias no ocupadas de una determinada actividad SIGUANET en la Universidad. 
Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_ua_numestanciasnoocupadas() ;
- select quest_ua_numestanciasnoocupadas();
';

CREATE FUNCTION quest_ua_numestanciasnoocupadas(tipo character varying, denominacion character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
 cuenta bigint;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT count(DISTINCT t.codigo) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo NOT IN (SELECT codigo FROM todaspersonas);' INTO cuenta;
 RETURN cuenta;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_numestanciasnoocupadas(tipo character varying, denominacion character varying) IS 'Obtiene el nº de estancias no ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS
SELECT quest_ua_numestanciasnoocupadas(''crue'',''DOCENCIA'');';

CREATE FUNCTION quest_ua_numestanciasocupadas() RETURNS bigint
    LANGUAGE sql
    AS $$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND codigo != '0000PB997';
$$;

COMMENT ON FUNCTION quest_ua_numestanciasocupadas() IS 'Obtiene el nº de estancias ocupadas de la Universidad. 
Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_ua_numestanciasocupadas() ;
- select quest_ua_numestanciasocupadas();';

CREATE FUNCTION quest_ua_numestanciasocupadas(integer) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1;
$_$;

COMMENT ON FUNCTION quest_ua_numestanciasocupadas(integer) IS 'Obtiene el nº de estancias ocupadas de una determinada actividad SIGUANET en la Universidad. 
Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_ua_numestanciasocupadas() ;
- select quest_ua_numestanciasocupadas();
';

CREATE FUNCTION quest_ua_numestanciasocupadas(integer[]) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = ANY ($1);
$_$;

COMMENT ON FUNCTION quest_ua_numestanciasocupadas(integer[]) IS 'Obtiene el nº de estancias ocupadas de una lista de actividades de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_plantabase_numestanciasocupadas(ARRAY[4,5,7,8]) ;
- select quest_plantabase_numestanciasocupadas(ARRAY[4,5,7,8,9,16]);
';

CREATE FUNCTION quest_ua_numestanciasocupadas(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND coddpto = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_numestanciasocupadas(character varying) IS 'Obtiene el nº total de estancias ocupadas adscritas a un departamento SIGUANET.
Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_ua_numestanciasocupadas(''B101'') ;
- select quest_ua_numestanciasocupadas(''B101'');';

CREATE FUNCTION quest_ua_numestanciasocupadas(tipo character varying, denominacion character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
 cuenta bigint;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT count(DISTINCT t.codigo) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo IN (SELECT codigo FROM todaspersonas);' INTO cuenta;
 RETURN cuenta;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_numestanciasocupadas(tipo character varying, denominacion character varying) IS 'Obtiene el nº de estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS
SELECT quest_ua_numestanciasocupadas(''crue'',''DOCENCIA'');';

CREATE FUNCTION quest_ua_numestanciasutiles() RETURNS bigint
    LANGUAGE sql
    AS $$
	SELECT count(codigo) FROM 
	(SELECT t.codigo from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE a.util = true
         group by t.codigo) AS foo;
$$;

COMMENT ON FUNCTION quest_ua_numestanciasutiles() IS 'Obtiene el nº de estancias útiles de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_ua_numestanciasutiles();
- select quest_ua_numestanciasutiles();
';

CREATE FUNCTION quest_ua_numestanciasutiles(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(codigo) FROM 
	(SELECT t.codigo from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE a.util = true
	 AND t.coddpto = upper($1)
         group by t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_ua_numestanciasutiles(character varying) IS 'Obtiene el nº de estancias útiles adscritas a un departamento SIGUANET. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_ua_numestanciasutiles(''B101'');
- select quest_ua_numestanciasutiles(''B101'');';

CREATE FUNCTION quest_ua_numexternos() RETURNS bigint
    LANGUAGE sql
    AS $$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalexternos) aux;
$$;

COMMENT ON FUNCTION quest_ua_numexternos() IS 'Obtiene el nº de externos trabajando en la Universidad. 
Se ejecuta de dos formas:
- select * from quest_ua_numexternos();
- select quest_ua_numexternos();
';

CREATE FUNCTION quest_ua_numexternos(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalexternos
	WHERE cod_dpto_sigua = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_numexternos(character varying) IS 'Obtiene el total de externos de un departamento SIGUANET. 
Se ejecuta de dos formas:
- select * from quest_ua_numexternos(''B101'');
- select quest_ua_numexternos(''B101'');';

CREATE FUNCTION quest_ua_numexternos(integer) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM personalexternos WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_ua_numexternos(tipo character varying, denominacion character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM personalexternos WHERE codigo IN
            (SELECT codigo FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_numexternos(tipo character varying, denominacion character varying) IS 'Obtiene el nº de empleados EXTERNOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS:
- SELECT quest_ua_numexternos(''crue'', ''DOCENCIA'');';

CREATE FUNCTION quest_ua_numexternosnoubicados() RETURNS bigint
    LANGUAGE sql
    AS $$
   SELECT count(*) FROM quest_checklist_personas WHERE esexterno = true AND locexterno = false;
$$;

COMMENT ON FUNCTION quest_ua_numexternosnoubicados() IS 'Obtiene el nº de externos con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_numexternosnoubicados();
- select quest_ua_numexternosnoubicados();
';

CREATE FUNCTION quest_ua_numexternosnoubicados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT v.nif) FROM quest_checklist_personas v, quest_personas2 tp
	WHERE tp.nif = v.nif
	AND v.esexterno = true
	AND v.locexterno = false
	AND tp.codigo = '0000PB997' 
	AND tp.cod_depto = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_numexternosnoubicados(character varying) IS 'Obtiene el nº de externos de un departamento SIGUANET con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_numexternosnoubicados(''B101'');
- select quest_ua_numexternosnoubicados(''B101'');';

CREATE FUNCTION quest_ua_numpas() RETURNS bigint
    LANGUAGE sql
    AS $$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalpas) aux;
$$;

COMMENT ON FUNCTION quest_ua_numpas() IS 'Obtiene el nº de PAS de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_ua_numpas();
- select quest_ua_numpas();
';

CREATE FUNCTION quest_ua_numpas(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalpas
	WHERE cod_unidad = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_numpas(character varying) IS 'Obtiene el total de PAS de un departamento SIGUANET. 
Se ejecuta de dos formas:
- select * from quest_ua_numpas(''B101'');
- select quest_ua_numpas(''B101'');';

CREATE FUNCTION quest_ua_numpas(integer) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM personalpas WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_ua_numpas(tipo character varying, denominacion character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM personalpas WHERE codigo IN
            (SELECT codigo FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_numpas(tipo character varying, denominacion character varying) IS 'Obtiene el nº de PAS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS:
- SELECT quest_ua_numpas(''crue'', ''DOCENCIA'');';

CREATE FUNCTION quest_ua_numpasnoubicados() RETURNS bigint
    LANGUAGE sql
    AS $$
   SELECT count(*) FROM quest_checklist_personas WHERE espas = true AND locpas = false;
$$;

COMMENT ON FUNCTION quest_ua_numpasnoubicados() IS 'Obtiene el nº de PAS con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_numpasnoubicados();
- select quest_ua_numpasnoubicados();
';

CREATE FUNCTION quest_ua_numpasnoubicados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT v.nif) FROM quest_checklist_personas v, quest_personas2 tp
	WHERE tp.nif = v.nif
	AND v.espas = true
	AND v.locpas = false
	AND tp.codigo = '0000PB997' 
	AND tp.cod_depto = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_numpasnoubicados(character varying) IS 'Obtiene el nº de PAS de un departamento SIGUANET con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_numpasnoubicados(''B101'');
- select quest_ua_numpasnoubicados(''B101'');';

CREATE FUNCTION quest_ua_numpdi() RETURNS bigint
    LANGUAGE sql
    AS $$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalpdi) aux;
$$;

COMMENT ON FUNCTION quest_ua_numpdi() IS 'Obtiene el nº de PDI de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_ua_numpdi();
- select quest_ua_numpdi();
';

CREATE FUNCTION quest_ua_numpdi(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalpdi
	WHERE cod_depto = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_numpdi(character varying) IS 'Obtiene el total de PDI de un departamento SIGUANET. 
Se ejecuta de dos formas:
- select * from quest_ua_numpdi(''B101'');
- select quest_ua_numpdi(''B101'');';

CREATE FUNCTION quest_ua_numpdi(integer) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM personalpdi WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_ua_numpdi(tipo character varying, denominacion character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM personalpdi WHERE codigo IN
            (SELECT codigo FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_numpdi(tipo character varying, denominacion character varying) IS 'Obtiene el nº de PDI ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS:
- SELECT quest_ua_numpdi(''crue'', ''DOCENCIA'');';

CREATE FUNCTION quest_ua_numpdicargos() RETURNS bigint
    LANGUAGE sql
    AS $$
   SELECT count(*) FROM quest_checklist_personas WHERE espdicargo = true;
$$;

COMMENT ON FUNCTION quest_ua_numpdicargos() IS 'Obtiene el nº de PDI con cargo de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_ua_numpdicargos();
- select quest_ua_numpdicargos();
';

CREATE FUNCTION quest_ua_numpdicargos(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalpdi_cargos p
	JOIN cargos_dpto cd ON p.cod_cargo = cd.cod_cargo
	WHERE cd.coddpto = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_numpdicargos(character varying) IS 'Obtiene el total de cargos pdi de un departamento SIGUANET. 
Se ejecuta de dos formas:
- select * from quest_ua_numpdicargos(''B101'');
- select quest_ua_numpdicargos(''B101'');';

CREATE FUNCTION quest_ua_numpdicargosnoubicados() RETURNS bigint
    LANGUAGE sql
    AS $$
   SELECT count(*) FROM quest_checklist_personas WHERE espdicargo = true AND locpdicargo = false;
$$;

COMMENT ON FUNCTION quest_ua_numpdicargosnoubicados() IS 'Obtiene el nº de PDI con cargo cuya ubicación de cargo es desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_numpdicargosnoubicados();
- select quest_ua_numpdicargosnoubicados();
';

CREATE FUNCTION quest_ua_numpdicargosnoubicados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$	
	SELECT count(DISTINCT p.cod_cargo)
	FROM personalpdi_cargos p 
	JOIN quest_personas2 tp ON p.nif = tp.nif
	JOIN cargos_dpto cd ON p.cod_cargo = cd.cod_cargo
	AND tp.codigo = '0000PB997'
	AND cd.coddpto = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_numpdicargosnoubicados(character varying) IS 'Obtiene el nº de cargos PDI de un departamento SIGUANET con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_numpdicargosnoubicados(''B101'');
- select quest_ua_numpdicargosnoubicados(''B101'');';

CREATE FUNCTION quest_ua_numpdinoubicados() RETURNS bigint
    LANGUAGE sql
    AS $$
   SELECT count(*) FROM quest_checklist_personas WHERE espdi = true AND locpdi = false;
$$;

COMMENT ON FUNCTION quest_ua_numpdinoubicados() IS 'Obtiene el nº de PDI con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_numpdinoubicados();
- select quest_ua_numpdinoubicados();
';

CREATE FUNCTION quest_ua_numpdinoubicados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT v.nif) FROM quest_checklist_personas v, quest_personas2 tp
	WHERE tp.nif = v.nif
	AND v.espdi = true
	AND v.locpdi = false
	AND tp.codigo = '0000PB997' 
	AND tp.cod_depto = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_numpdinoubicados(character varying) IS 'Obtiene el nº de PDI de un departamento SIGUANET con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_numpdinoubicados(''B101'');
- select quest_ua_numpdinoubicados(''B101'');';

CREATE FUNCTION quest_ua_numpersonas() RETURNS bigint
    LANGUAGE sql
    AS $$
   SELECT count(DISTINCT nif) FROM todaspersonas;
$$;

COMMENT ON FUNCTION quest_ua_numpersonas() IS 'Obtiene el nº de personas (PAS, PDI, becarios y externos) que trabajan en la Universidad.
Se ejecuta de dos formas:
- select * from quest_ua_numpersonas();
- select quest_ua_numpersonas();
';

CREATE FUNCTION quest_ua_numpersonas(character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$DECLARE
    cuenta int8;
    adscripcion varchar;	
BEGIN
   adscripcion := upper($1);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT nif) INTO cuenta FROM todaspersonas
   WHERE cod_depto = adscripcion;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_ua_numpersonas(character varying) IS 'Obtiene el nº de personas de un departamento sigua
SINTAXIS:
- SELECT quest_ua_numpersonas(''B101'');';

CREATE FUNCTION quest_ua_numpersonas(integer) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM todaspersonas WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_ua_numpersonas(tipo character varying, denominacion character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM todaspersonas WHERE codigo IN
            (SELECT codigo FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || ');' INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_numpersonas(tipo character varying, denominacion character varying) IS 'Obtiene el nº de personas ubicadas en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS:
- SELECT quest_ua_numpersonas(''crue'', ''DOCENCIA'');';

CREATE FUNCTION quest_ua_numpersonasnoubicados() RETURNS bigint
    LANGUAGE sql
    AS $$
SELECT quest_ua_numpasnoubicados() + quest_ua_numpdinoubicados() + quest_ua_numpdicargosnoubicados() +
       quest_ua_numbecariosnoubicados() + quest_ua_numexternosnoubicados();
$$;

COMMENT ON FUNCTION quest_ua_numpersonasnoubicados() IS 'Obtiene el nº de personas con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_numpersonasnoubicados();
- select quest_ua_numpersonasnoubicados();
';

CREATE FUNCTION quest_ua_numpersonasnoubicados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
SELECT quest_ua_numpasnoubicados($1) + quest_ua_numpdinoubicados($1) + quest_ua_numpdicargosnoubicados($1) +
       quest_ua_numbecariosnoubicados($1) + quest_ua_numexternosnoubicados($1);
$_$;

COMMENT ON FUNCTION quest_ua_numpersonasnoubicados(character varying) IS 'Obtiene el nº de personas de un departamento SIGUANET con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_numpersonasnoubicados(''B101'');
- select quest_ua_numpersonasnoubicados(''B101'');';

CREATE FUNCTION quest_ua_obteneractividadessigua() RETURNS SETOF public.actividades
    LANGUAGE sql
    AS $$
	SELECT a.*
	 FROM  actividades a JOIN (SELECT actividad FROM todasestancias GROUP BY actividad) e ON a.codactividad = e.actividad  
	 ORDER BY a.txt_actividad;
$$;

COMMENT ON FUNCTION quest_ua_obteneractividadessigua() IS 'Obtiene todas las actividades SIGUANET
para las que existen estancias.
Se ejecuta de dos formas:
- select * from quest_ua_obteneractividadessigua();
- select quest_ua_obteneractividadessigua();';

CREATE FUNCTION quest_ua_obteneradmonnoocupados() RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $$
SELECT * FROM quest_estancias
WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
AND actividad = 8;
$$;

CREATE FUNCTION quest_ua_obteneradmonnoocupados(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias
WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
AND actividad = 8
AND coddpto = upper($1);
$_$;

CREATE FUNCTION quest_ua_obteneradmonocupados() RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $$
SELECT * FROM quest_estancias
WHERE codigo IN (SELECT codigo FROM todaspersonas)
AND actividad = 8;
$$;

CREATE FUNCTION quest_ua_obteneradmonocupados(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias
WHERE codigo IN (SELECT codigo FROM todaspersonas)
AND actividad = 8
AND coddpto = upper($1);
$_$;

CREATE FUNCTION quest_ua_obtenerdepartamentossigua() RETURNS SETOF quest_departamentos
    LANGUAGE sql
    AS $$
	SELECT DISTINCT d.* 
	FROM  quest_departamentos d JOIN todasestancias e ON d.cod = e.coddpto  
	WHERE d.es_dpto = true OR d.es_unidad = true
	ORDER BY d.txt;
$$;

COMMENT ON FUNCTION quest_ua_obtenerdepartamentossigua() IS 'Obtiene todos los departamentos SIGUANET
que tienen estancias adscritas.
ATENCIÓN: NO se incluyen centros ya que cualquier facultad, escuela o intituto debe tener su correspondencia
en la tabla de unidades, y es el código de unidad el que se usa para adscribir una estancia.
Se ejecuta de dos formas:
- select * from quest_ua_obtenerdepartamentossigua();
- select quest_ua_obtenerdepartamentossigua();';

CREATE FUNCTION quest_ua_obtenerdepartamentossigua2() RETURNS SETOF record
    LANGUAGE plpgsql
    AS $$
DECLARE
   micursor refcursor;
   fila record;
BEGIN
  OPEN micursor FOR
	SELECT t.coddpto, d.txt_dpto_sigua 
	FROM todasestancias t, departamentossigua d 
	WHERE t.coddpto = d.cod_dpto_sigua
	GROUP BY t.coddpto,d.txt_dpto_sigua
	ORDER BY d.txt_dpto_sigua;
  FETCH micursor INTO fila;
  WHILE FOUND LOOP
    RETURN NEXT fila;
    FETCH micursor INTO fila;
  END LOOP;
  RETURN;

END
$$;

COMMENT ON FUNCTION quest_ua_obtenerdepartamentossigua2() IS 'Obtiene un listado de los departamentos sigua que tienen estancias.. 
Se ejecuta de la siguiente manera:
SELECT * FROM quest_ua_obtenerdepartamentossigua() AS (dpto varchar, txt_dpto varchar);';

CREATE FUNCTION quest_ua_obtenerdespachosnoocupados() RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $$
SELECT * FROM quest_estancias
WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
AND actividad = 7;
$$;

CREATE FUNCTION quest_ua_obtenerdespachosnoocupados(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND coddpto = upper($1);
$_$;

CREATE FUNCTION quest_ua_obtenerdespachosocupados() RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $$
SELECT * FROM quest_estancias
WHERE codigo IN (SELECT codigo FROM todaspersonas)
AND actividad = 7;
$$;

CREATE FUNCTION quest_ua_obtenerdespachosocupados(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND coddpto = upper($1);
$_$;

CREATE FUNCTION quest_ua_obtenerestancias(character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila quest_estancias%ROWTYPE;
    adscripcion varchar;	
BEGIN
   adscripcion := upper($1);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   FOR fila IN 
     SELECT * FROM quest_estancias WHERE coddpto = adscripcion
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_ua_obtenerestancias(character varying) IS 'Obtiene todas las estancias de un departamento sigua
Ejemplo:
SELECT * FROM quest_ua_obtenerestancias(''B101'');';

CREATE FUNCTION quest_ua_obtenerestancias(integer) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila quest_estancias%ROWTYPE;
    act integer;
BEGIN
   act := $1;
   IF act NOT IN (SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION '% no es una actividad en la tabla actividades.', act;
   END IF;

   FOR fila IN 
     SELECT * FROM quest_estancias WHERE actividad = act
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_ua_obtenerestancias(integer) IS 'Obtiene las estancias designadas para un uso SIGUANET
SINTAXIS:
SELECT * FROM quest_ua_obtenerestancias(8);';

CREATE FUNCTION quest_ua_obtenerestancias(integer, character varying) RETURNS SETOF public.todasestancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila todasestancias%ROWTYPE;
    adscripcion varchar;	
BEGIN
   adscripcion := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   IF $1 NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'El actividad % no está definida en la tabla Actividades.', $1;
   END IF;

   FOR fila IN 
	SELECT t.*
	FROM todasestancias t, departamentossigua d, actividades a
	WHERE 
	t.coddpto = d.cod_dpto_sigua AND
	t.actividad = a.codactividad AND
	t.actividad = $1 AND
	t.coddpto = $2
	ORDER BY t.codigo   
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END

$_$;

COMMENT ON FUNCTION quest_ua_obtenerestancias(integer, character varying) IS 'Obtiene el nº de estancias de una actividad sigua y de un dpto sigua
Ejemplo:
SELECT * FROM quest_ua_obtenerestancias(8,''B101'');
';

CREATE FUNCTION quest_ua_obtenerestancias(tipo character varying, denominacion character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $$
DECLARE
  validacion integer;
  c refcursor;
  estancia quest_estancias%ROWTYPE;
  tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
        WHEN 'activresum' THEN 'denogrupo'
        WHEN 'crue' THEN 'denocrue'
        WHEN 'u21' THEN 'denou21'
        END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || ';';
  LOOP
   FETCH c INTO estancia;
   EXIT WHEN NOT FOUND;
   RETURN NEXT estancia;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_obtenerestancias(tipo character varying, denominacion character varying) IS 'Obtiene las estancias de un tipo de grupo de actividad (crue, u21,activresum) y una denominación de grupo de actividad.
SINTAXIS
SELECT quest_ua_obtenerestancias(''crue'',''DOCENCIA'');';

CREATE FUNCTION quest_ua_obtenerestancias(character varying, character varying, character varying) RETURNS SETOF public.todasestancias
    LANGUAGE plpgsql
    AS $_$DECLARE
    fila todasestancias%ROWTYPE;
    agrupa varchar;
    adscripcion varchar;
    denoactividad varchar;	
BEGIN

agrupa := lower($1);
adscripcion  := upper($3);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

IF agrupa = 'crue' THEN
	denoactividad := upper($2);
        IF denoactividad NOT IN ( SELECT crue FROM actividades) THEN
	   RAISE EXCEPTION 'El actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;

	FOR fila IN 
		SELECT t.* 
		FROM todasestancias t, departamentossigua d, actividades  a
		WHERE 
		t.coddpto = d.cod_dpto_sigua AND
		t.actividad = a.codactividad AND
		a.crue = upper($2) AND
		t.coddpto = adscripcion
		ORDER BY t.codigo 
         LOOP
		RETURN NEXT fila;
	END LOOP;
ELSEIF agrupa = 'u21' THEN
	denoactividad := upper($2);
        IF denoactividad NOT IN ( SELECT u21 FROM actividades) THEN
	   RAISE EXCEPTION 'El actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
	FOR fila IN 
		SELECT t.* 
		FROM todasestancias t, departamentossigua d, actividades  a
		WHERE 
		t.coddpto = d.cod_dpto_sigua AND
		t.actividad = a.codactividad AND
		a.u21 = upper($2) AND
		t.coddpto = adscripcion
		ORDER BY t.codigo 
        LOOP
		RETURN NEXT fila;
	END LOOP;
ELSEIF agrupa = 'activresum' THEN
	denoactividad := $2;
        IF denoactividad NOT IN ( SELECT activresum FROM actividades) THEN
	   RAISE EXCEPTION 'El actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
	FOR fila IN 
		SELECT t.* 
		FROM todasestancias t, departamentossigua d, actividades  a
		WHERE 
		t.coddpto = d.cod_dpto_sigua AND
		t.actividad = a.codactividad AND
		a.activresum = $2 AND
		t.coddpto = adscripcion
		ORDER BY t.codigo 
        LOOP
		RETURN NEXT fila;
	END LOOP;
ELSE
     RAISE exception 'No existe un campo valido con ese nombre: (%). Debe de ser u21, activresum o crue ', $1;

END IF;

RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_ua_obtenerestancias(character varying, character varying, character varying) IS 'Obtiene registros de todasestancias de un dpto sigua, un grupo de actividad sigua y su valor correspondiente.
SINTAXIS
SELECT * FROM quest_ua_obtenerestancias(''crue'',''DOCENCIA'',''B101'');';

CREATE FUNCTION quest_ua_obtenerestanciasdocentes() RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $$
	SELECT * FROM quest_estancias WHERE upper(denogrupo) = 'DOCENCIA';
$$;

CREATE FUNCTION quest_ua_obtenerestanciasdocentes(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias 
	WHERE upper(denogrupo) = 'DOCENCIA'
	AND coddpto = upper($1);
$_$;

CREATE FUNCTION quest_ua_obtenerestanciasnoocupadas(integer) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1;
$_$;

CREATE FUNCTION quest_ua_obtenerestanciasnoocupadas(tipo character varying, denominacion character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $$
DECLARE
 validacion integer;
 c refcursor;
 estancia quest_estancias%ROWTYPE;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 SELECT INTO tipo_ CASE lower(tipo)
  WHEN 'activresum' THEN 'denogrupo'
  WHEN 'crue' THEN 'denocrue'
  WHEN 'u21' THEN 'denou21'
 END;
 OPEN c FOR EXECUTE 'SELECT * FROM quest_estancias 
                      WHERE lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || 
                    ' AND codigo NOT IN (SELECT codigo FROM todaspersonas);';
 LOOP
  FETCH c INTO estancia;
  EXIT WHEN NOT FOUND;
  RETURN NEXT estancia;
 END LOOP;
 RETURN;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_obtenerestanciasnoocupadas(tipo character varying, denominacion character varying) IS 'Obtiene las estancias no ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS
SELECT * FROM quest_ua_obtenerestanciasnoocupadas(''crue'',''DOCENCIA'');';

CREATE FUNCTION quest_ua_obtenerestanciasocupadas() RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $$
	SELECT *
	FROM quest_estancias 
	WHERE (actividad <= 50 OR actividad = 99)
	AND codigo IN (SELECT codigo FROM todaspersonas)
	AND codigo != '0000PB997';

$$;

COMMENT ON FUNCTION quest_ua_obtenerestanciasocupadas() IS 'Obtiene un dataset con todas las estancias ocupadas de la UA.
SINTAXIS:
- SELECT * FROM quest_ua_estanciasocupadas();';

CREATE FUNCTION quest_ua_obtenerestanciasocupadas(integer[]) RETURNS SETOF public.todasestancias
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = ANY ($1);
$_$;

CREATE FUNCTION quest_ua_obtenerestanciasocupadas(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND coddpto = upper($1);

$_$;

COMMENT ON FUNCTION quest_ua_obtenerestanciasocupadas(character varying) IS 'Obtiene el conjunto de registros de todas las estancias ocupadas adscritas a un departamento SIGUANET.
SINTAXIS:
- SELECT * FROM quest_ua_obtenerestanciasocupadas(''B101'');';

CREATE FUNCTION quest_ua_obtenerestanciasocupadas(integer) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1;
$_$;

CREATE FUNCTION quest_ua_obtenerestanciasocupadas(tipo character varying, denominacion character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $$
DECLARE
 validacion integer;
 c refcursor;
 estancia quest_estancias%ROWTYPE;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 SELECT INTO tipo_ CASE lower(tipo)
  WHEN 'activresum' THEN 'denogrupo'
  WHEN 'crue' THEN 'denocrue'
  WHEN 'u21' THEN 'denou21'
 END;
 OPEN c FOR EXECUTE 'SELECT * FROM quest_estancias 
                      WHERE lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || 
                    ' AND codigo IN (SELECT codigo FROM todaspersonas);';
 LOOP
  FETCH c INTO estancia;
  EXIT WHEN NOT FOUND;
  RETURN NEXT estancia;
 END LOOP;
 RETURN;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_obtenerestanciasocupadas(tipo character varying, denominacion character varying) IS 'Obtiene las estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS
SELECT * FROM quest_ua_obtenerestanciasocupadas(''crue'',''DOCENCIA'');';

CREATE FUNCTION quest_ua_obtenerestanciasutiles(character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila quest_estancias%ROWTYPE;
    adscripcion varchar;	
BEGIN
   adscripcion := upper($1);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   FOR fila IN 
	SELECT * FROM quest_estancias t JOIN actividades a ON t.actividad = a.codactividad 
	WHERE a.util = true 
	AND t.coddpto = adscripcion
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_ua_obtenerestanciasutiles(character varying) IS 'Obtiene todas las estancias útiles de un departamento sigua
Ejemplo:
SELECT * FROM quest_ua_obtenerestancias(''B101'');';

CREATE FUNCTION quest_ua_obtenergruposactividad(tipo character varying) RETURNS SETOF character varying
    LANGUAGE plpgsql
    AS $$
DECLARE
 validacion integer;
 c refcursor;
 grupo varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, '') INTO validacion;
 --Si el tipo de grupo es correcto (i.e. activresum, crue, u21) la función de validación devuelve 2 (tipo correcto, denominación de grupo desconocida)
 IF validacion = 2 THEN
  OPEN c FOR EXECUTE 'SELECT a.' || lower(tipo) ||
	             ' FROM  actividades a JOIN (SELECT actividad FROM todasestancias GROUP BY actividad) e ON a.codactividad = e.actividad' ||
	             ' GROUP BY 1 ORDER BY 1;';
  LOOP
   FETCH c INTO grupo;
   EXIT WHEN NOT FOUND;
   RETURN NEXT grupo;
  END LOOP;
  RETURN;
 ELSE
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_obtenergruposactividad(tipo character varying) IS 'Obtiene todos los grupos de actividad del tipo especificado 
(i.e. activresum, crue, u21) para los que existen estancias.
Se ejecuta de dos formas:
- select * from quest_ua_obtenergruposactividad(''activresum'');
- select quest_ua_obtenergruposactividad(''crue'');';

CREATE FUNCTION quest_ua_obtenerpersonas() RETURNS SETOF quest_checklist_personas
    LANGUAGE sql
    AS $$
   SELECT * FROM quest_checklist_personas;
$$;

COMMENT ON FUNCTION quest_ua_obtenerpersonas() IS 'Esta función utiliza una vista llamada
quest_checklist_personas, que devuelve las personas de la ua, así como si es pas, pdi, cargo pdi, externo, becario, o la combinación de alguna de ellas.
SINTAXIS
- SELECT * FROM quest_ua_obtenerpersonas()';

CREATE FUNCTION quest_ua_obtenerpersonas(character varying) RETURNS SETOF record
    LANGUAGE plpgsql
    AS $_$
DECLARE
   micursor refcursor;
   adscripcion varchar;
   fila record;

BEGIN
   adscripcion := upper($1);

   IF adscripcion NOT IN (SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

-- apertura del cursor
  OPEN micursor FOR
 
SELECT v.* FROM quest_checklist_personas v, todaspersonas tp
WHERE tp.nif = v.nif 
AND tp.cod_depto = adscripcion;  

  FETCH micursor INTO fila;
  WHILE FOUND LOOP
    RETURN NEXT fila;
    FETCH micursor INTO fila;
  END LOOP;
  RETURN;

END;
$_$;

COMMENT ON FUNCTION quest_ua_obtenerpersonas(character varying) IS 'Obtiene un listado de personas de un dpto sigua y además si es pas,pdi,becario,externo o alguna de sus combinaciones.
SINTAXIS:
SELECT * FROM quest_ua_obtenerpersonas(''B101'') AS (nif varchar, apellido1 varchar, apellido2 varchar, nombre varchar,espas bool, espdi bool, esbecario bool, esexterno bool);
';

CREATE FUNCTION quest_ua_obtenerplantas() RETURNS SETOF text
    LANGUAGE sql
    AS $$
	SELECT planta FROM quest_plantasbase() ORDER BY indice;
$$;

CREATE FUNCTION quest_ua_obtenerplantas(character varying) RETURNS SETOF text
    LANGUAGE sql
    AS $_$
	SELECT DISTINCT planta FROM
	(SELECT p.planta FROM quest_plantasbase() p JOIN todasestancias e ON p.planta = substring(e.codigo FROM 5 FOR 2)
	WHERE e.coddpto = $1
	ORDER BY p.indice) AS foo;
$_$;

CREATE FUNCTION quest_ua_obtenerplantas(integer) RETURNS SETOF text
    LANGUAGE sql
    AS $_$
	 SELECT p.planta FROM quest_plantasbase() p JOIN 
          (SELECT substring(codigo FROM 5 FOR 2) AS planta FROM todasestancias WHERE actividad = $1) e
          ON p.planta = e.planta
          GROUP BY p.planta, p.indice
	  ORDER BY p.indice;
$_$;

CREATE FUNCTION quest_ua_obtenerplantas(tipo character varying, denominacion character varying) RETURNS SETOF text
    LANGUAGE plpgsql
    AS $$
DECLARE
  validacion integer;
  c refcursor;
  planta text;
  tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
        WHEN 'activresum' THEN 'denogrupo'
        WHEN 'crue' THEN 'denocrue'
        WHEN 'u21' THEN 'denou21'
        END;
  OPEN c FOR EXECUTE 'SELECT p.planta FROM quest_plantasbase() p JOIN 
                       (SELECT substring(codigo FROM 5 FOR 2) AS planta FROM quest_estancias 
                         WHERE lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || ') e
                       ON p.planta = e.planta GROUP BY p.planta, p.indice ORDER BY p.indice;';
  LOOP
   FETCH c INTO planta;
   EXIT WHEN NOT FOUND;
   RETURN NEXT planta;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_obtenerplantas(tipo character varying, denominacion character varying) IS 'Obtiene las plantas de la universidad en las que existen estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS
SELECT * FROM quest_ua_obtenerplantas(''crue'',''DOCENCIA'');';

CREATE FUNCTION quest_ua_obtenerplantasedificio() RETURNS SETOF quest_plantaedificio
    LANGUAGE sql
    AS $$
	SELECT * FROM quest_plantasedificio() ORDER BY zona, edificio, indice;
$$;

CREATE FUNCTION quest_ua_obtenerzonas() RETURNS SETOF public.zonas
    LANGUAGE sql
    AS $$
	SELECT * FROM zonas ORDER BY txt_zona;
$$;

CREATE FUNCTION quest_ua_obtenerzonas(character varying) RETURNS SETOF public.zonas
    LANGUAGE sql
    AS $_$
	SELECT DISTINCT z.* FROM zonas z JOIN todasestancias e ON z.cod_zona = substring(e.codigo FROM 1 FOR 2)
	WHERE e.coddpto = $1
	ORDER BY txt_zona;
$_$;

CREATE FUNCTION quest_ua_obtenerzonas(integer) RETURNS SETOF public.zonas
    LANGUAGE sql
    AS $_$
	SELECT DISTINCT z.* FROM zonas z 
         JOIN (SELECT substring(codigo FROM 1 FOR 2) AS cod_zona FROM todasestancias WHERE actividad = $1 GROUP BY 1) e 
         ON z.cod_zona = e.cod_zona
	 ORDER BY txt_zona;
$_$;

CREATE FUNCTION quest_ua_obtenerzonas(tipo character varying, denominacion character varying) RETURNS SETOF public.zonas
    LANGUAGE plpgsql
    AS $$
DECLARE
  validacion integer;
  c refcursor;
  zona zonas%ROWTYPE;
  tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
        WHEN 'activresum' THEN 'denogrupo'
        WHEN 'crue' THEN 'denocrue'
        WHEN 'u21' THEN 'denou21'
        END;
  OPEN c FOR EXECUTE 'SELECT DISTINCT z.* FROM zonas z 
                       JOIN (SELECT substring(codigo FROM 1 FOR 2) AS cod_zona FROM quest_estancias 
                              WHERE lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || ' GROUP BY 1) e 
                       ON z.cod_zona = e.cod_zona ORDER BY txt_zona;';
  LOOP
   FETCH c INTO zona;
   EXIT WHEN NOT FOUND;
   RETURN NEXT zona;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_obtenerzonas(tipo character varying, denominacion character varying) IS 'Obtiene los campus o sedes en los que existen estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS
SELECT * FROM quest_ua_obtenerzonas(''crue'',''DOCENCIA'');';

CREATE FUNCTION quest_ua_porcentajebecariosnoubicados() RETURNS bigint
    LANGUAGE sql
    AS $$
   SELECT (quest_ua_numbecariosnoubicados() * 100) / quest_ua_numbecarios();
$$;

COMMENT ON FUNCTION quest_ua_porcentajebecariosnoubicados() IS 'Obtiene el % de becarios con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_porcentajebecariosnoubicados();
- select quest_ua_porcentajebecariosnoubicados();
';

CREATE FUNCTION quest_ua_porcentajebecariosnoubicados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT (quest_ua_numbecariosnoubicados($1) * 100) / quest_ua_numbecarios($1);
$_$;

COMMENT ON FUNCTION quest_ua_porcentajebecariosnoubicados(character varying) IS 'Obtiene el % de becarios de un departamento SIGUANET con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_porcentajebecariosnoubicados(''B101'');
- select quest_ua_porcentajebecariosnoubicados(''B101'');';

CREATE FUNCTION quest_ua_porcentajeexternosnoubicados() RETURNS bigint
    LANGUAGE sql
    AS $$
   SELECT (quest_ua_numexternosnoubicados() * 100) / quest_ua_numexternos();
$$;

COMMENT ON FUNCTION quest_ua_porcentajeexternosnoubicados() IS 'Obtiene el % de externos con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_porcentajeexternosnoubicados();
- select quest_ua_porcentajeexternosnoubicados();
';

CREATE FUNCTION quest_ua_porcentajeexternosnoubicados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT (quest_ua_numexternosnoubicados($1) * 100) / quest_ua_numexternos($1);
$_$;

COMMENT ON FUNCTION quest_ua_porcentajeexternosnoubicados(character varying) IS 'Obtiene el % de externos de un departamento SIGUANET con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_porcentajeexternosnoubicados(''B101'');
- select quest_ua_porcentajeexternosnoubicados(''B101'');';

CREATE FUNCTION quest_ua_porcentajepasnoubicados() RETURNS bigint
    LANGUAGE sql
    AS $$
   SELECT (quest_ua_numpasnoubicados() * 100) / quest_ua_numpas();
$$;

COMMENT ON FUNCTION quest_ua_porcentajepasnoubicados() IS 'Obtiene el % de PAS con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_porcentajepasnoubicados();
- select quest_ua_porcentajepasnoubicados();
';

CREATE FUNCTION quest_ua_porcentajepasnoubicados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT (quest_ua_numpasnoubicados($1) * 100) / quest_ua_numpas($1);
$_$;

COMMENT ON FUNCTION quest_ua_porcentajepasnoubicados(character varying) IS 'Obtiene el % de PAS de un departamento SIGUANET con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_porcentajepasnoubicados(''B101'');
- select quest_ua_porcentajepasnoubicados(''B101'');';

CREATE FUNCTION quest_ua_porcentajepdicargosnoubicados() RETURNS bigint
    LANGUAGE sql
    AS $$
   SELECT (quest_ua_numpdicargosnoubicados() * 100) / quest_ua_numpdicargos();
$$;

COMMENT ON FUNCTION quest_ua_porcentajepdicargosnoubicados() IS 'Obtiene el % de PDI con cargo cuya ubicación de cargo es desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_porcentajepdicargosnoubicados();
- select quest_ua_porcentajepdicargosnoubicados();
';

CREATE FUNCTION quest_ua_porcentajepdicargosnoubicados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT (quest_ua_numpdicargosnoubicados($1) * 100) / quest_ua_numpdicargos($1);
$_$;

COMMENT ON FUNCTION quest_ua_porcentajepdicargosnoubicados(character varying) IS 'Obtiene el % de PDI con cargo en un departamento SIGUANET cuya ubicación de cargo es desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_porcentajepdicargosnoubicados(''B101'');
- select quest_ua_porcentajepdicargosnoubicados(''B101'');';

CREATE FUNCTION quest_ua_porcentajepdinoubicados() RETURNS bigint
    LANGUAGE sql
    AS $$
   SELECT (quest_ua_numpdinoubicados() * 100) / quest_ua_numpdi();
$$;

COMMENT ON FUNCTION quest_ua_porcentajepdinoubicados() IS 'Obtiene el % de PDI con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_porcentajepdinoubicados();
- select quest_ua_porcentajepdinoubicados();
';

CREATE FUNCTION quest_ua_porcentajepdinoubicados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT (quest_ua_numpdinoubicados($1) * 100) / quest_ua_numpdi($1);
$_$;

COMMENT ON FUNCTION quest_ua_porcentajepdinoubicados(character varying) IS 'Obtiene el % de PDI de un departamento SIGUANET con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_porcentajepdinoubicados(''B101'');
- select quest_ua_porcentajepdinoubicados(''B101'');';

CREATE FUNCTION quest_ua_porcentajepersonasnoubicados() RETURNS bigint
    LANGUAGE sql
    AS $$
   SELECT (quest_ua_numpersonasnoubicados() * 100) / quest_ua_numpersonas();
$$;

COMMENT ON FUNCTION quest_ua_porcentajepersonasnoubicados() IS 'Obtiene el % de personas con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_porcentajepersonasnoubicados();
- select quest_ua_porcentajepersonasnoubicados();
';

CREATE FUNCTION quest_ua_porcentajepersonasnoubicados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT (quest_ua_numpersonasnoubicados($1) * 100) / quest_ua_numpersonas($1);
$_$;

COMMENT ON FUNCTION quest_ua_porcentajepersonasnoubicados(character varying) IS 'Obtiene el % de personas de un departamento SIGUANET con ubicación desconocida. 
Se ejecuta de dos formas:
- select * from quest_ua_porcentajepersonasnoubicados(''B101'');
- select quest_ua_porcentajepersonasnoubicados(''B101'');';

CREATE FUNCTION quest_ua_superficie(integer) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    superficie float8;
BEGIN
   IF $1 NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'El actividad % no está definida en la tabla Actividades.', $1;
   END IF;

   SELECT sum(st_area(geometria)) INTO superficie FROM todasestancias WHERE actividad = $1;
   RETURN superficie;

END;

$_$;

COMMENT ON FUNCTION quest_ua_superficie(integer) IS 'Obtiene la superficie de una determinada actividad SIGUANET
SINTAXIS:
- SELECT * FROM quest_ua_superficie(50);
- SELECT quest_ua_superficie(50);
';

CREATE FUNCTION quest_ua_superficie(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$DECLARE
    superficie float8;
    adscripcion varchar;
BEGIN
   adscripcion := upper($1);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO superficie FROM todasestancias WHERE coddpto = adscripcion;
   RETURN superficie;

END;

$_$;

COMMENT ON FUNCTION quest_ua_superficie(character varying) IS 'Obtiene la suma de las superficies de todas las estancias adscritas a un departamento sigua.
Ejemplo: 
- SELECT quest_ua_superficie(''B101'');
- SELECT * FROM quest_ua_superficie(''B101'');';

CREATE FUNCTION quest_ua_superficie(tipo character varying, denominacion character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
 superficie double precision;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT sum(st_area(geometria))  FROM 
  (SELECT t.geometria FROM todasestancias t, actividades a 
   WHERE t.actividad = a.codactividad
   AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ') AS foo' INTO superficie;
 RETURN superficie;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_superficie(tipo character varying, denominacion character varying) IS 'Obtiene la superficie que ocupan las estancias de un tipo de grupo de actividad (crue, u21,activresum) y una denominación de grupo de actividad.
SINTAXIS
SELECT quest_ua_superficie(''crue'',''DOCENCIA'');';

CREATE FUNCTION quest_ua_superficie(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    sumag float8;	
BEGIN
    adscripcion := upper($2);
-- Control de errores
   IF $1 NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'El actividad % no está definida en la tabla Actividades.', $1;
   END IF;
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;


   SELECT sum(st_area(geometria)) INTO sumag FROM todasestancias  
   WHERE coddpto = adscripcion 
   AND actividad = $1;

   RETURN sumag;

   
END;

$_$;

COMMENT ON FUNCTION quest_ua_superficie(integer, character varying) IS 'Obtiene la superficie de un departamento sigua y una actividad sigua. 
Sintaxis:
SELECT quest_ua_superficie(8, ''B101'');
';

CREATE FUNCTION quest_ua_superficie(character varying, character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    agrupa varchar;
    sumag float8;
    denoactividad varchar;	
BEGIN
   adscripcion := upper($3);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

    agrupa := lower($1);

IF agrupa = 'crue' THEN
	denoactividad := upper($2);
        IF denoactividad NOT IN ( SELECT crue FROM actividades) THEN
	   RAISE EXCEPTION 'El actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;

	SELECT sum(st_area(geometria)) INTO sumag FROM todasestancias t, departamentossigua d, actividades a 
	WHERE t.coddpto = d.cod_dpto_sigua
	AND t.actividad = a.codactividad
	AND t.coddpto = adscripcion 
	AND a.crue = upper($2);
	RETURN sumag;

ELSEIF agrupa = 'u21' THEN
	denoactividad := upper($2);
        IF denoactividad NOT IN ( SELECT u21 FROM actividades) THEN
	   RAISE EXCEPTION 'El actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
	SELECT sum(st_area(geometria)) INTO sumag FROM todasestancias t, departamentossigua d, actividades a 
	WHERE t.coddpto = d.cod_dpto_sigua
	AND t.actividad = a.codactividad
	AND t.coddpto = adscripcion 
	AND a.u21 = upper($2);
	RETURN sumag;

ELSEIF agrupa = 'activresum' THEN
	denoactividad := $2;
        IF denoactividad NOT IN ( SELECT activresum FROM actividades) THEN
	   RAISE EXCEPTION 'El actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
	SELECT sum(st_area(geometria)) INTO sumag FROM todasestancias t, departamentossigua d, actividades a 
	WHERE t.coddpto = d.cod_dpto_sigua
	AND t.actividad = a.codactividad
	AND t.coddpto = adscripcion 
	AND a.activresum = $2;
	RETURN sumag;
ELSE
     RAISE exception 'No existe un campo valido con ese nombre: (%). Debe de ser u21, activresum o crue ', $1;
END IF;
END;
$_$;

COMMENT ON FUNCTION quest_ua_superficie(character varying, character varying, character varying) IS 'Obtiene la superficie de un grupo de actividad (crue, u21,activresum) y un determinado departamento SIGUANET
Ejemplo:
SELECT quest_ua_superficie(''crue'',''docencia'',''B101'');
';

CREATE FUNCTION quest_ua_superficieadmonnoocupados() RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
    sumag float8;
BEGIN

   SELECT sum(st_area(t.geometria)) INTO sumag
   FROM todasestancias t
   WHERE t.codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND t.actividad = 8;

	RETURN sumag;

END;
$$;

CREATE FUNCTION quest_ua_superficieadmonnoocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    adscripcion varchar;	
BEGIN
   adscripcion := upper($1);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_ua_superficieadmonnoocupados(character varying) IS 'Obtiene la superficie de las administraciones no ocupadas de un dtpo sigua.
SINTAXIS: 
- SELECT quest_ua_superficieadmonnoocupados(''B101'');
- SELECT * FROM quest_ua_superficieadmonnoocupados(''B101'');';

CREATE FUNCTION quest_ua_superficieadmonocupados() RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
    sumag float8;
BEGIN

   SELECT sum(st_area(t.geometria)) INTO sumag
   FROM todasestancias t
   WHERE t.codigo IN (SELECT codigo FROM todaspersonas)
   AND t.actividad = 8;

	RETURN sumag;

END;
$$;

CREATE FUNCTION quest_ua_superficieadmonocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    adscripcion varchar;	
BEGIN
   adscripcion := upper($1);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_ua_superficieadmonocupados(character varying) IS 'Obtiene la superfice de las administraciones ocupadas de un dpto sigua.
SINTAXIS:
- SELECT * FROM quest_ua_superficieadmonocupados(''B101'');
- SELECT quest_ua_superficieadmonocupados(''B101'');';

CREATE FUNCTION quest_ua_superficiedespachosnoocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    adscripcion varchar;	
BEGIN
   adscripcion := upper($1);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_ua_superficiedespachosnoocupados(character varying) IS 'Obtiene la superficie de los despachos no ocupados de un dtpo sigua.
SINTAXIS: 
- SELECT quest_ua_superficiedespachosnoocupados(''B101'');
- SELECT * FROM quest_ua_superficiedespachosnoocupados(''B101'');';

CREATE FUNCTION quest_ua_superficiedespachosocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    adscripcion varchar;	
BEGIN
   adscripcion := upper($1);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_ua_superficiedespachosocupados(character varying) IS 'Obtiene la superfice de los despachos ocupados de un dpto sigua.
SINTAXIS:
- SELECT * FROM quest_ua_superficiedespachosocupados(''B101'');
- SELECT quest_ua_superficiedespachosocupados(''B101'');';

CREATE FUNCTION quest_ua_superficiedocente() RETURNS double precision
    LANGUAGE sql
    AS $$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE upper(a.activresum) = 'DOCENCIA';
$$;

COMMENT ON FUNCTION quest_ua_superficiedocente() IS 'Obtiene la superficie total de las estancias docentes de la Universidad.
Se ejecuta de dos formas:
- select * from quest_ua_superficiedocente();
- select quest_ua_superficiedocente();
';

CREATE FUNCTION quest_ua_superficiedocente(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE upper(a.activresum) = 'DOCENCIA'
	AND t.coddpto = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_superficiedocente(character varying) IS 'Obtiene la superficie total de las estancias docentes adscritas a un departamento SIGUANET.
Se ejecuta de dos formas:
- select * from quest_ua_superficiedocente(''B101'');
- select quest_ua_superficiedocente(''B101'');';

CREATE FUNCTION quest_ua_superficieestanciasnoocupadas(integer) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1;
$_$;

COMMENT ON FUNCTION quest_ua_superficieestanciasnoocupadas(integer) IS 'Obtiene la superficie de las estancias no ocupadas de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_ua_superficieestanciasnoocupadas(50);
- SELECT quest_ua_superficieestanciasnoocupadas(50);
';

CREATE FUNCTION quest_ua_superficieestanciasnoocupadas(tipo character varying, denominacion character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
 superficie double precision;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT sum(st_area(t.geometria)) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo NOT IN (SELECT codigo FROM todaspersonas);' INTO superficie;
 RETURN superficie;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_superficieestanciasnoocupadas(tipo character varying, denominacion character varying) IS 'Obtiene la superficie de las estancias no ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS
SELECT quest_ua_superficieestanciasnoocupadas(''crue'',''DOCENCIA'');';

CREATE FUNCTION quest_ua_superficieestanciasocupadas() RETURNS double precision
    LANGUAGE sql
    AS $$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND codigo != '0000PB997';
$$;

COMMENT ON FUNCTION quest_ua_superficieestanciasocupadas() IS 'Obtiene la superficie de las  estancias ocupadas de la Universidad. 
Se ejecuta de dos formas:
- select quest_ua_superficieestanciasocupadas();';

CREATE FUNCTION quest_ua_superficieestanciasocupadas(integer) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1;
$_$;

COMMENT ON FUNCTION quest_ua_superficieestanciasocupadas(integer) IS 'Obtiene la superficie de las estancias ocupadas de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_ua_superficieestanciasocupadas(50);
- SELECT quest_ua_superficieestanciasocupadas(50);
';

CREATE FUNCTION quest_ua_superficieestanciasocupadas(integer[]) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = ANY ($1);
$_$;

COMMENT ON FUNCTION quest_ua_superficieestanciasocupadas(integer[]) IS 'Obtiene la superficie de las estancias ocupadas de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_ua_superficieestanciasocupadas(ARRAY[7,8,50]);
- SELECT quest_ua_superficieestanciasocupadas(ARRAY[7,8,50]);
';

CREATE FUNCTION quest_ua_superficieestanciasocupadas(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND coddpto = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_superficieestanciasocupadas(character varying) IS 'Obtiene la superficie de las  estancias ocupadas adscritas a un departamento SIGUANET. 
Se ejecuta de dos formas:
- select * from quest_ua_superficieestanciasocupadas(''B101'') ;
- select quest_ua_superficieestanciasocupadas(''B101'');';

CREATE FUNCTION quest_ua_superficieestanciasocupadas(integer[], character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = ANY ($1)
	AND coddpto = upper($2);
$_$;

COMMENT ON FUNCTION quest_ua_superficieestanciasocupadas(integer[], character varying) IS 'Obtiene la superficie de las estancias ocupadas y adscritas a un departamento de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_ua_superficieestanciasocupadas(ARRAY[7,8,50], ''B101'');
- SELECT quest_ua_superficieestanciasocupadas(ARRAY[7,8,50], ''B101'');';

CREATE FUNCTION quest_ua_superficieestanciasocupadas(tipo character varying, denominacion character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
 superficie double precision;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT sum(st_area(t.geometria)) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo IN (SELECT codigo FROM todaspersonas);' INTO superficie;
 RETURN superficie;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_superficieestanciasocupadas(tipo character varying, denominacion character varying) IS 'Obtiene la superficie de las estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS
SELECT quest_ua_superficieestanciasocupadas(''crue'',''DOCENCIA'');';

CREATE FUNCTION quest_ua_superficieestanciasocupadasbec(integer) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM becarios)
	AND actividad = $1;
$_$;

COMMENT ON FUNCTION quest_ua_superficieestanciasocupadasbec(integer) IS 'Obtiene la superficie de las estancias ocupadas por becarios de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_ua_superficieestanciasocupadasbecarios(7);
- SELECT quest_ua_superficieestanciasocupadasbecarios(7);
';

CREATE FUNCTION quest_ua_superficieestanciasocupadasbec(integer[]) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM becarios)
	AND actividad = ANY ($1);
$_$;

COMMENT ON FUNCTION quest_ua_superficieestanciasocupadasbec(integer[]) IS 'Obtiene la superficie de las estancias ocupadas por becarios de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_ua_superficieestanciasocupadasbecarios(ARRAY[4,5,7]);
- SELECT quest_ua_superficieestanciasocupadasbecarios(ARRAY[4,5,7]);
';

CREATE FUNCTION quest_ua_superficieestanciasocupadasbec(integer[], character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM becarios)
	AND actividad = ANY ($1)
	AND coddpto = upper($2);
$_$;

COMMENT ON FUNCTION quest_ua_superficieestanciasocupadasbec(integer[], character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por becarios de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_ua_superficieestanciasocupadasbecarios(ARRAY[8,9,16], ''B101'');
- SELECT quest_ua_superficieestanciasocupadasbecarios(ARRAY[8,9,16], ''B101'');';

CREATE FUNCTION quest_ua_superficieestanciasocupadasext(integer) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalexternos)
	AND actividad = $1;
$_$;

COMMENT ON FUNCTION quest_ua_superficieestanciasocupadasext(integer) IS 'Obtiene la superficie de las estancias ocupadas por personal externo de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_ua_superficieestanciasocupadasexternos(7);
- SELECT quest_ua_superficieestanciasocupadasexternos(7);
';

CREATE FUNCTION quest_ua_superficieestanciasocupadasext(integer[]) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalexternos)
	AND actividad = ANY ($1);
$_$;

COMMENT ON FUNCTION quest_ua_superficieestanciasocupadasext(integer[]) IS 'Obtiene la superficie de las estancias ocupadas por personal externo de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_ua_superficieestanciasocupadasexternos(ARRAY[7,8]);
- SELECT quest_ua_superficieestanciasocupadasexternos(ARRAY[7,8]);
';

CREATE FUNCTION quest_ua_superficieestanciasocupadasext(integer[], character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalexternos)
	AND actividad = ANY ($1)
	AND coddpto = upper($2);
$_$;

COMMENT ON FUNCTION quest_ua_superficieestanciasocupadasext(integer[], character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por externos de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_ua_superficieestanciasocupadasext(ARRAY[8,9,16], ''B101'');
- SELECT quest_ua_superficieestanciasocupadasext(ARRAY[8,9,16], ''B101'');';

CREATE FUNCTION quest_ua_superficieestanciasocupadaspas(integer) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpas)
	AND actividad = $1;
$_$;

COMMENT ON FUNCTION quest_ua_superficieestanciasocupadaspas(integer) IS 'Obtiene la superficie de las estancias ocupadas por PAS de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_ua_superficieestanciasocupadaspas(8);
- SELECT quest_ua_superficieestanciasocupadaspas(8);
';

CREATE FUNCTION quest_ua_superficieestanciasocupadaspas(integer[]) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpas)
	AND actividad = ANY ($1);
$_$;

COMMENT ON FUNCTION quest_ua_superficieestanciasocupadaspas(integer[]) IS 'Obtiene la superficie de las estancias ocupadas por PAS de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_ua_superficieestanciasocupadaspas(ARRAY[8,9,16]);
- SELECT quest_ua_superficieestanciasocupadaspas(ARRAY[8,9,16]);
';

CREATE FUNCTION quest_ua_superficieestanciasocupadaspas(integer[], character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpas)
	AND actividad = ANY ($1)
	AND coddpto = upper($2);
$_$;

COMMENT ON FUNCTION quest_ua_superficieestanciasocupadaspas(integer[], character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por PAS de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_ua_superficieestanciasocupadaspas(ARRAY[8,9,16], ''B101'');
- SELECT quest_ua_superficieestanciasocupadaspas(ARRAY[8,9,16], ''B101'');';

CREATE FUNCTION quest_ua_superficieestanciasocupadaspdi(integer) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpdi)
	AND actividad = $1;
$_$;

COMMENT ON FUNCTION quest_ua_superficieestanciasocupadaspdi(integer) IS 'Obtiene la superficie de las estancias ocupadas por PDI de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_ua_superficieestanciasocupadaspdi(7);
- SELECT quest_ua_superficieestanciasocupadaspdi(7);
';

CREATE FUNCTION quest_ua_superficieestanciasocupadaspdi(integer[]) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpdi)
	AND actividad = ANY ($1);
$_$;

COMMENT ON FUNCTION quest_ua_superficieestanciasocupadaspdi(integer[]) IS 'Obtiene la superficie de las estancias ocupadas por PDI de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_ua_superficieestanciasocupadaspdi(ARRAY[4,5,7]);
- SELECT quest_ua_superficieestanciasocupadaspdi(ARRAY[4,5,7]);
';

CREATE FUNCTION quest_ua_superficieestanciasocupadaspdi(integer[], character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpdi)
	AND actividad = ANY ($1)
	AND coddpto = upper($2);
$_$;

COMMENT ON FUNCTION quest_ua_superficieestanciasocupadaspdi(integer[], character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por PDI de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_ua_superficieestanciasocupadaspdi(ARRAY[8,9,16], ''B101'');
- SELECT quest_ua_superficieestanciasocupadaspdi(ARRAY[8,9,16], ''B101'');';

CREATE FUNCTION quest_ua_superficieutil() RETURNS double precision
    LANGUAGE sql
    AS $$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE a.util = true;
$$;

COMMENT ON FUNCTION quest_ua_superficieutil() IS 'Obtiene la superficie útil de la Universidad.
Se ejecuta de dos formas:
- select * from quest_ua_superficieutil();
- select quest_ua_superficieutil();
';

CREATE FUNCTION quest_ua_superficieutil(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE a.util = true
	AND t.coddpto = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_superficieutil(character varying) IS 'Obtiene la superficie útil de un departamento SIGUANET.
Se ejecuta de dos formas:
- select * from quest_ua_superficieutil(''B101'');
- select quest_ua_superficieutil(''B101'');';

CREATE FUNCTION quest_ua_ubicaciones() RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $$
   SELECT * FROM quest_ubicaciones WHERE actividad <= 50 OR actividad = 99;
$$;

COMMENT ON FUNCTION quest_ua_ubicaciones() IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de la Universidad.
SINTAXIS
- SELECT * FROM quest_ua_ubicaciones()';

CREATE FUNCTION quest_ua_ubicaciones(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$SELECT DISTINCT v.* FROM quest_ubicaciones v JOIN quest_personas2 tp ON v.nif = tp.nif WHERE tp.cod_depto = upper($1);

$_$;

COMMENT ON FUNCTION quest_ua_ubicaciones(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de un departamento SIGUANET.
SINTAXIS
- SELECT * FROM quest_ua_ubicaciones(''B101'')';

CREATE FUNCTION quest_ua_ubicaciones(integer) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones WHERE actividad = $1;
$_$;

COMMENT ON FUNCTION quest_ua_ubicaciones(integer) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET.
SINTAXIS
- SELECT * FROM quest_ua_ubicaciones(7)';

CREATE FUNCTION quest_ua_ubicaciones(tipo character varying, denominacion character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_ubicaciones(tipo character varying, denominacion character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para las personas ubicadas en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS:
- SELECT * FROM quest_ua_ubicaciones(''crue'', ''DOCENCIA'');';

CREATE FUNCTION quest_ua_ubicacionesbecarios() RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $$
   SELECT * FROM quest_ubicaciones
   WHERE esbecario = true 
   AND locbecario = true
   AND reftbl = 'becarios'
   AND (actividad <= 50 OR actividad = 99);
$$;

COMMENT ON FUNCTION quest_ua_ubicacionesbecarios() IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de la Universidad.
La condición where matiza que sólo devuelve estancias con PDI
SINTAXIS
- SELECT * FROM quest_ua_ubicacionesbecarios()';

CREATE FUNCTION quest_ua_ubicacionesbecarios(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_becarios p ON v.nif = p.nif
	WHERE v.esbecario = true 
	AND v.locbecario = true
	AND v.reftbl = 'becarios'
	AND p.cod_depto_centro_subunidad = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_ubicacionesbecarios(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociación becario/estancia de los becarios adscritos a un departamento SIGUANET.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionesbecarios(''B101'')';

CREATE FUNCTION quest_ua_ubicacionesbecarios(integer) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 
   AND esbecario = true 
   AND locbecario = true
   AND reftbl = 'becarios';
$_$;

COMMENT ON FUNCTION quest_ua_ubicacionesbecarios(integer) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion becario/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionesbecarios(7)';

CREATE FUNCTION quest_ua_ubicacionesbecarios(tipo character varying, denominacion character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND esbecario = true 
                       AND locbecario = true
                       AND reftbl = ''becarios'';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_ubicacionesbecarios(tipo character varying, denominacion character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los BECARIOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS:
- SELECT * FROM quest_ua_ubicacionesbecarios(''crue'', ''DOCENCIA'');';

CREATE VIEW quest_ubicacionespendientes AS
    SELECT DISTINCT p.nif, p.apellido1, p.apellido2, p.nombre, p.espas, p.locpas, p.espdi, p.locpdi, p.espdicargo, p.locpdicargo, p.esbecario, p.locbecario, p.esexterno, p.locexterno, 0 AS gid, te.codigo, public.st_astext(te.geometria) AS wkt, public.st_srid(te.geometria) AS srid, "substring"((te.codigo)::text, 1, 6) AS codplantaedif, "substring"((te.codigo)::text, 5, 2) AS enumplanta, (((e.txt_edificio)::text || ' '::text) || "substring"((te.codigo)::text, 5, 2)) AS denoplanta, ((e.cod_zona)::text || (e.cod_edificio)::text) AS codedificio, e.txt_edificio AS denoedificio, z.cod_zona AS codzona, z.txt_zona AS denozona, te.actividad, a.txt_actividad AS denoactividad, a.activresum AS denogrupo, a.crue AS denocrue, a.u21 AS denou21, te.coddpto, ds.txt AS denodpto, ds.es_centro, ds.es_dpto, ds.es_unidad, te.denominaci AS denoestancia, te.observacio AS observaestancia, tp.reftbl FROM public.todasestancias te, public.edificios e, public.zonas z, public.actividades a, quest_departamentos ds, quest_personas tp, quest_checklist_personas p WHERE ((((((((te.codigo)::text = '0000PB997'::text) AND ("substring"((te.codigo)::text, 1, 4) = ((e.cod_zona)::text || (e.cod_edificio)::text))) AND ("substring"((te.codigo)::text, 1, 2) = (z.cod_zona)::text)) AND (te.actividad = a.codactividad)) AND ((te.coddpto)::text = (ds.cod)::text)) AND ((te.codigo)::text = (tp.codigo)::text)) AND ((tp.nif)::text = (p.nif)::text)) ORDER BY "substring"((te.codigo)::text, 1, 6), p.apellido1, p.apellido2, p.nif, te.codigo, public.st_astext(te.geometria), public.st_srid(te.geometria), "substring"((te.codigo)::text, 5, 2), (((e.txt_edificio)::text || ' '::text) || "substring"((te.codigo)::text, 5, 2)), ((e.cod_zona)::text || (e.cod_edificio)::text), e.txt_edificio, z.cod_zona, z.txt_zona, te.actividad, a.txt_actividad, a.activresum, a.crue, a.u21, te.coddpto, ds.txt, ds.es_centro, ds.es_dpto, ds.es_unidad, te.denominaci, te.observacio, p.nombre, p.espas, p.locpas, p.espdi, p.locpdi, p.espdicargo, p.locpdicargo, p.esbecario, p.locbecario, p.esexterno, p.locexterno, 0::integer, tp.reftbl;

COMMENT ON VIEW quest_ubicacionespendientes IS 'Permite crear objetos de tipo ubicación pendiente, es decir, todas las asociaciones persona/0000PB997.';

CREATE FUNCTION quest_ua_ubicacionesbecariospendientes() RETURNS SETOF quest_ubicacionespendientes
    LANGUAGE sql
    AS $$
   SELECT * FROM quest_ubicacionespendientes WHERE esbecario = true AND locbecario = false AND reftbl = 'becarios';
$$;

COMMENT ON FUNCTION quest_ua_ubicacionesbecariospendientes() IS 'Esta función utiliza una vista llamada
quest_ubicacionespendientes, que devuelve todos los pares de asociacion becario/0000PB997 de la Universidad.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionesbecariospendientes()';

CREATE FUNCTION quest_ua_ubicacionesbecariospendientes(character varying) RETURNS SETOF quest_ubicacionespendientes
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicacionespendientes v
	JOIN quest_becarios p ON v.nif = p.nif
	WHERE esbecario = true 
	AND locbecario = false
	AND reftbl = 'becarios'
	AND p.cod_depto_centro_subunidad = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_ubicacionesbecariospendientes(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicacionespendientes, que devuelve todos los pares de asociación becario/0000PB997 de los becarios adscritos a un departamento SIGUANET.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionesbecariospendientes(''B101'')';

CREATE FUNCTION quest_ua_ubicacionescargos() RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $$
   SELECT * FROM quest_ubicaciones 
   WHERE espdicargo = true 
   AND locpdicargo = true
   AND reftbl = 'personalpdi_cargos'   
   AND (actividad <= 50 OR actividad = 99);
$$;

COMMENT ON FUNCTION quest_ua_ubicacionescargos() IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de la Universidad.
La condición where matiza que sólo devuelve estancias con CARGOS
SINTAXIS
- SELECT * FROM quest_ua_ubicacionescargos()';

CREATE FUNCTION quest_ua_ubicacionescargos(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalpdi_cargos p ON v.nif = p.nif
	JOIN cargos_dpto cd ON p.cod_cargo = cd.cod_cargo
	WHERE v.espdicargo = true 
	AND v.locpdicargo = true
	AND v.reftbl = 'personalpdi_cargos'
	AND cd.coddpto = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_ubicacionescargos(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociación pdicargo/estancia de los cargos adscritos a un departamento SIGUANET.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionescargos(''B101'')';

CREATE FUNCTION quest_ua_ubicacionescargospendientes() RETURNS SETOF quest_ubicacionespendientes
    LANGUAGE sql
    AS $$
   SELECT * FROM quest_ubicacionespendientes WHERE espdicargo = true AND locpdicargo = false AND reftbl = 'personalpdi_cargos';
$$;

COMMENT ON FUNCTION quest_ua_ubicacionescargospendientes() IS 'Esta función utiliza una vista llamada
quest_ubicacionespendientes, que devuelve todos los pares de asociacion pdi_cargo/0000PB997 de la Universidad.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionescargospendientes()';

CREATE FUNCTION quest_ua_ubicacionescargospendientes(character varying) RETURNS SETOF quest_ubicacionespendientes
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicacionespendientes v
	JOIN quest_personalpdi_cargos p ON v.nif = p.nif
	JOIN cargos_dpto cd ON p.cod_cargo = cd.cod_cargo
	WHERE espdicargo = true 
	AND locpdicargo = false
	AND reftbl = 'personalpdi_cargos'
	AND cd.coddpto = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_ubicacionescargospendientes(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicacionespendientes, que devuelve todos los pares de asociación pdicargo/0000PB997 de los cargos adscritos a un departamento SIGUANET.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionescargospendientes(''B101'')';

CREATE FUNCTION quest_ua_ubicacionesexternos() RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $$
   SELECT * FROM quest_ubicaciones
   WHERE esexterno = true
   AND locexterno = true
   AND reftbl = 'personalexternos'
   AND (actividad <= 50 OR actividad = 99);
$$;

COMMENT ON FUNCTION quest_ua_ubicacionesexternos() IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de la Universidad.
La condición where matiza que sólo devuelve estancias DE EXTERNOS
SINTAXIS
- SELECT * FROM quest_ua_ubicacionesexternos()';

CREATE FUNCTION quest_ua_ubicacionesexternos(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalexternos p ON v.nif = p.nif
	WHERE v.esexterno = true 
	AND v.locexterno = true
	AND v.reftbl = 'personalexternos'
	AND p.cod_dpto_sigua = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_ubicacionesexternos(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion externo/estancia de los externos adscritos a un departamento SIGUANET.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionesexternos(''B101'')';

CREATE FUNCTION quest_ua_ubicacionesexternos(integer) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 
   AND esexterno = true 
   AND locexterno = true
   AND reftbl = 'personalexternos';
$_$;

COMMENT ON FUNCTION quest_ua_ubicacionesexternos(integer) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion externo/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionesexternos(7)';

CREATE FUNCTION quest_ua_ubicacionesexternos(tipo character varying, denominacion character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND esexterno = true 
                       AND locexterno = true
                       AND reftbl = ''personalexternos'';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_ubicacionesexternos(tipo character varying, denominacion character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los empleados EXTERNOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS:
- SELECT * FROM quest_ua_ubicacionesexternos(''crue'', ''DOCENCIA'');';

CREATE FUNCTION quest_ua_ubicacionesexternospendientes() RETURNS SETOF quest_ubicacionespendientes
    LANGUAGE sql
    AS $$
   SELECT * FROM quest_ubicacionespendientes WHERE esexterno = true AND locexterno = false AND reftbl = 'personalexternos';
$$;

COMMENT ON FUNCTION quest_ua_ubicacionesexternospendientes() IS 'Esta función utiliza una vista llamada
quest_ubicacionespendientes, que devuelve todos los pares de asociacion externo/0000PB997 de la Universidad.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionesexternospendientes()';

CREATE FUNCTION quest_ua_ubicacionesexternospendientes(character varying) RETURNS SETOF quest_ubicacionespendientes
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicacionespendientes v
	JOIN quest_personalexternos p ON v.nif = p.nif
	WHERE esexterno = true 
	AND locexterno = false
	AND reftbl = 'personalexternos'
	AND p.cod_dpto_sigua = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_ubicacionesexternospendientes(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicacionespendientes, que devuelve todos los pares de asociacion externo/0000PB997 de los externos adscritos a un departamento SIGUANET.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionesexternospendientes(''B101'')';

CREATE FUNCTION quest_ua_ubicacionespas() RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $$
	SELECT * FROM quest_ubicaciones 
	WHERE espas = true
	AND locpas = true
	AND reftbl = 'personalpas'
	AND (actividad <= 50 OR actividad = 99);
$$;

COMMENT ON FUNCTION quest_ua_ubicacionespas() IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de la Universidad.
La condición where matiza que sólo devuelve estancias con pas
SINTAXIS
- SELECT * FROM quest_ua_ubicacionespas()';

CREATE FUNCTION quest_ua_ubicacionespas(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalpas p ON v.nif = p.nif
	WHERE v.espas = true 
	AND v.locpas = true
	AND v.reftbl = 'personalpas'
	AND p.cod_unidad = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_ubicacionespas(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion pas/estancia de los pas adscritos a un departamento SIGUANET.
La condición where matiza que sólo devuelve estancias con pas
SINTAXIS
- SELECT * FROM quest_ua_ubicacionespas(''B101'')';

CREATE FUNCTION quest_ua_ubicacionespas(integer) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 
   AND espas = true 
   AND locpas = true
   AND reftbl = 'personalpas';
$_$;

COMMENT ON FUNCTION quest_ua_ubicacionespas(integer) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion pas/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionespas(7)';

CREATE FUNCTION quest_ua_ubicacionespas(tipo character varying, denominacion character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND espas = true 
                       AND locpas = true
                       AND reftbl = ''personalpas'';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_ubicacionespas(tipo character varying, denominacion character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los PAS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS:
- SELECT * FROM quest_ua_ubicacionespas(''crue'', ''DOCENCIA'');';

CREATE FUNCTION quest_ua_ubicacionespaspendientes() RETURNS SETOF quest_ubicacionespendientes
    LANGUAGE sql
    AS $$
   SELECT * FROM quest_ubicacionespendientes WHERE espas = true AND locpas = false AND reftbl = 'personalpas';
$$;

COMMENT ON FUNCTION quest_ua_ubicacionespaspendientes() IS 'Esta función utiliza una vista llamada
quest_ubicacionespendientes, que devuelve todos los pares de asociacion pas/0000PB997 de la Universidad.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionespaspendientes()';

CREATE FUNCTION quest_ua_ubicacionespaspendientes(character varying) RETURNS SETOF quest_ubicacionespendientes
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicacionespendientes v
	JOIN quest_personalpas p ON v.nif = p.nif
	WHERE espas = true 
	AND locpas = false
	AND reftbl = 'personalpas'
	AND p.cod_unidad = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_ubicacionespaspendientes(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicacionespendientes, que devuelve todos los pares de asociación pas/0000PB997 de los pas adscritos a un departamento SIGUANET.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionespaspendientes(''B101'')';

CREATE FUNCTION quest_ua_ubicacionespdi() RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $$
   SELECT * FROM quest_ubicaciones 
   WHERE espdi = true 
   AND reftbl = 'personalpdi'
   AND (actividad <= 50 OR actividad = 99);
$$;

COMMENT ON FUNCTION quest_ua_ubicacionespdi() IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de la Universidad.
La condición where matiza que sólo devuelve estancias con PDI
SINTAXIS
- SELECT * FROM quest_ua_ubicacionespdi()';

CREATE FUNCTION quest_ua_ubicacionespdi(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalpdi p ON v.nif = p.nif
	WHERE v.espdi = true 
	AND v.locpdi = true
	AND v.reftbl = 'personalpdi'
	AND p.cod_depto = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_ubicacionespdi(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociación pdi/estancia de los pdi adscritos a un departamento SIGUANET.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionespdi(''B101'')';

CREATE FUNCTION quest_ua_ubicacionespdi(integer) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 
   AND espdi = true 
   AND locpdi = true
   AND reftbl = 'personalpdi';
$_$;

COMMENT ON FUNCTION quest_ua_ubicacionespdi(integer) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion pdi/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionespdi(7)';

CREATE FUNCTION quest_ua_ubicacionespdi(tipo character varying, denominacion character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND espdi = true 
                       AND locpdi = true
                       AND reftbl = ''personalpdi'';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_ua_ubicacionespdi(tipo character varying, denominacion character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los PDI ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS:
- SELECT * FROM quest_ua_ubicacionespdi(''crue'', ''DOCENCIA'');';

CREATE FUNCTION quest_ua_ubicacionespdipendientes() RETURNS SETOF quest_ubicacionespendientes
    LANGUAGE sql
    AS $$
   SELECT * FROM quest_ubicacionespendientes WHERE espdi = true AND locpdi = false AND reftbl = 'personalpdi';
$$;

COMMENT ON FUNCTION quest_ua_ubicacionespdipendientes() IS 'Esta función utiliza una vista llamada
quest_ubicacionespendientes, que devuelve todos los pares de asociacion pdi/0000PB997 de la Universidad.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionespdipendientes()';

CREATE FUNCTION quest_ua_ubicacionespdipendientes(character varying) RETURNS SETOF quest_ubicacionespendientes
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicacionespendientes v
	JOIN quest_personalpdi p ON v.nif = p.nif
	WHERE espdi = true 
	AND locpdi = false
	AND reftbl = 'personalpdi'
	AND p.cod_depto = upper($1);
$_$;

COMMENT ON FUNCTION quest_ua_ubicacionespdipendientes(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicacionespendientes, que devuelve todos los pares de asociación pdi/0000PB997 de los pdi adscritos a un departamento SIGUANET.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionespdipendientes(''B101'')';

CREATE FUNCTION quest_ua_ubicacionespendientes() RETURNS SETOF quest_ubicacionespendientes
    LANGUAGE sql
    AS $$
   SELECT * FROM quest_ubicacionespendientes;
$$;

COMMENT ON FUNCTION quest_ua_ubicacionespendientes() IS 'Esta función utiliza una vista llamada
quest_ubicacionespendientes, que devuelve todos los pares de asociacion persona/0000PB997 de la Universidad.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionespendientes()';

CREATE FUNCTION quest_ua_ubicacionespendientes(character varying) RETURNS SETOF quest_ubicacionespendientes
    LANGUAGE sql
    AS $_$
	SELECT DISTINCT v.* FROM quest_ubicacionespendientes v
	JOIN quest_personas2 p ON v.nif = p.nif
	WHERE p.cod_depto = upper($1)
	ORDER BY v.apellido1, apellido2;
$_$;

COMMENT ON FUNCTION quest_ua_ubicacionespendientes(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicacionespendientes, que devuelve todos los pares de asociacion persona/0000PB997 de los empleados adscritos a un departamento SIGUANET.
SINTAXIS
- SELECT * FROM quest_ua_ubicacionespendientes(''B101'')';

CREATE FUNCTION quest_zona_densidad(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_zona_superficieestanciasocupadas(ARRAY[4,5,7,8,9,16], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE actividad = ANY (ARRAY[4,5,7,8,9,16])
										       AND substring(codigo from 1 for 2) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_zona_densidad(character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en un campus/sede de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_zona_densidad(''00'');
- select quest_zona_densidad(''00'');
';

CREATE FUNCTION quest_zona_densidad(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_zona_superficieestanciasocupadas(ARRAY[4,5,7,8,9,16], upper($1), upper($2)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE actividad = ANY (ARRAY[4,5,7,8,9,16])
						     AND coddpto = upper($1)
						     AND substring(codigo from 1 for 2) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_zona_densidad(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en un departamento SIGUANET y en un campus/sede de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_zona_densidad(''B101'', ''00'');
- select quest_zona_densidad(''B101'', ''00'');';

CREATE FUNCTION quest_zona_densidad(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_zona_superficieestanciasocupadas($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM todaspersonas WHERE codigo IN (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 2) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_zona_densidad(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en las estancias de una determinada actividad SIGUANET para un campus o sede de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_zona_densidad(4, ''00'');
- select quest_zona_densidad(8, ''00'');';

CREATE FUNCTION quest_zona_densidad(tipo character varying, denominacion character varying, zona character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  SELECT quest_zona_superficieestanciasocupadas(tipo, denominacion, zona) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM todaspersonas WHERE codigo IN
            (SELECT codigo FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
          '   AND substring(codigo FROM 1 FOR 2) = ' || quote_literal(zona) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;   
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_densidad(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad, en un campus o sede. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_zona_densidad(''crue'', ''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_densidadbecarios(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_zona_superficieestanciasocupadasbec(ARRAY[4,5,7], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM becarios)
										       AND actividad = ANY (ARRAY[4,5,7])
										       AND substring(codigo from 1 for 2) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_zona_densidadbecarios(character varying) IS 'Obtiene los m2 de espacio de trabajo por becario en un campus/sede de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_zona_densidadbecarios(''00'');
- select quest_zona_densidadbecarios(''00'');
';

CREATE FUNCTION quest_zona_densidadbecarios(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_zona_superficieestanciasocupadasbec(ARRAY[4,5,7], $1, $2) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM becarios)
						     AND actividad = ANY (ARRAY[4,5,7])
						     AND coddpto = upper($1)
						     AND substring(codigo FROM 1 FOR 2) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_zona_densidadbecarios(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por becarios en un departamento SIGUANET y en un campus/sede de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_zona_densidadbecarios(''B101'', ''00'');
- select quest_zona_densidadbecarios(''B101'', ''00'');';

CREATE FUNCTION quest_zona_densidadbecarios(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_zona_superficieestanciasocupadasbec($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM becarios WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM becarios)
                                           AND actividad = $1
                                           AND substring(codigo from 1 for 2) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_zona_densidadbecarios(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por becario en las estancias de una determinada actividad SIGUANET para un campus o sede. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_zona_densidadbecarios(4, ''00'');
- select quest_zona_densidadbecarios(8, ''00'');';

CREATE FUNCTION quest_zona_densidadbecarios(tipo character varying, denominacion character varying, zona character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_zona_superficieestanciasocupadasbec(actlist, zona) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM becarios WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 2) = ' || quote_literal(zona) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_densidadbecarios(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene los m2 de espacio de trabajo por BECARIO en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un campus o sede. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_zona_densidadbecarios(''crue'', ''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_densidadexternos(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_zona_superficieestanciasocupadasext(ARRAY[7,8], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalexternos)
										       AND actividad = ANY (ARRAY[7,8])
										       AND substring(codigo from 1 for 2) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_zona_densidadexternos(character varying) IS 'Obtiene los m2 de espacio de trabajo por externo en un campus/sede de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_zona_densidadexternos(''00'');
- select quest_zona_densidadexternos(''00'');
';

CREATE FUNCTION quest_zona_densidadexternos(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_zona_superficieestanciasocupadasext(ARRAY[7,8], $1, $2) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM personalexternos)
						     AND actividad = ANY (ARRAY[7,8])
						     AND coddpto = upper($1)
						     AND substring(codigo FROM 1 FOR 2) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_zona_densidadexternos(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por externos en un departamento SIGUANET y en un campus/sede de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_zona_densidadexternos(''B101'', ''00'');
- select quest_zona_densidadexternos(''B101'', ''00'');';

CREATE FUNCTION quest_zona_densidadexternos(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_zona_superficieestanciasocupadasext($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM personalexternos WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalexternos)
                                           AND actividad = $1
                                           AND substring(codigo from 1 for 2) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_zona_densidadexternos(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por externo en las estancias de una determinada actividad SIGUANET para un campus o sede. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_zona_densidadexternos(4, ''00'');
- select quest_zona_densidadexternos(8, ''00'');';

CREATE FUNCTION quest_zona_densidadexternos(tipo character varying, denominacion character varying, zona character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_zona_superficieestanciasocupadasext(actlist, zona) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM personalexternos WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 2) = ' || quote_literal(zona) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_densidadexternos(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado EXTERNO en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un campus o sede. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_zona_densidadexternos(''crue'', ''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_densidadpas(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_zona_superficieestanciasocupadaspas(ARRAY[4,5,8,9,16], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpas)
										       AND actividad = ANY (ARRAY[4,5,8,9,16])
										       AND substring(codigo from 1 for 2) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_zona_densidadpas(character varying) IS 'Obtiene los m2 de espacio de trabajo por PAS en un campus/sede de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_zona_densidadpas(''00'');
- select quest_zona_densidadpas(''00'');
';

CREATE FUNCTION quest_zona_densidadpas(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_zona_superficieestanciasocupadaspas(ARRAY[4,5,8,9,16], $1, $2) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM personalpas)
						     AND actividad = ANY (ARRAY[4,5,8,9,16])
						     AND coddpto = upper($1)
						     AND substring(codigo FROM 1 FOR 2) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_zona_densidadpas(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por PAS en un departamento SIGUANET y en un campus/sede de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_zona_densidadpas(''B101'', ''00'');
- select quest_zona_densidadpas(''B101'', ''00'');';

CREATE FUNCTION quest_zona_densidadpas(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_zona_superficieestanciasocupadaspas($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM personalpas WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpas)
                                           AND actividad = $1
                                           AND substring(codigo from 1 for 2) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_zona_densidadpas(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado PAS en las estancias de una determinada actividad SIGUANET para un campus o sede. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_zona_densidadpas(4, ''00'');
- select quest_zona_densidadpas(8, ''00'');';

CREATE FUNCTION quest_zona_densidadpas(tipo character varying, denominacion character varying, zona character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_zona_superficieestanciasocupadaspas(actlist, zona) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM personalpas WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 2) = ' || quote_literal(zona) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_densidadpas(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado tipo PAS en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un campus o sede. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_zona_densidadpas(''crue'', ''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_densidadpdi(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_zona_superficieestanciasocupadaspdi(ARRAY[4,5,7], upper($1)) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpdi)
										       AND actividad = ANY (ARRAY[4,5,7])
										       AND substring(codigo from 1 for 2) = upper($1)));
$_$;

COMMENT ON FUNCTION quest_zona_densidadpdi(character varying) IS 'Obtiene los m2 de espacio de trabajo por PDI en un campus/sede de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_zona_densidadpdi(''00'');
- select quest_zona_densidadpdi(''00'');
';

CREATE FUNCTION quest_zona_densidadpdi(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT quest_zona_superficieestanciasocupadaspdi(ARRAY[4,5,7], $1, $2) / 
        (SELECT count(nif) FROM todaspersonas WHERE codigo IN 
						    (SELECT codigo FROM todasestancias 
						     WHERE codigo IN (SELECT codigo FROM personalpdi)
						     AND actividad = ANY (ARRAY[4,5,7])
						     AND coddpto = upper($1)
						     AND substring(codigo FROM 1 FOR 2) = upper($2)));
$_$;

COMMENT ON FUNCTION quest_zona_densidadpdi(character varying, character varying) IS 'Obtiene los m2 de espacio de trabajo por PDI en un departamento SIGUANET y en un campus/sede de la Universidad. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_zona_densidadpdi(''B101'', ''00'');
- select quest_zona_densidadpdi(''B101'', ''00'');';

CREATE FUNCTION quest_zona_densidadpdi(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
BEGIN
 SELECT quest_zona_superficieestanciasocupadaspdi($1, $2) INTO superficie;
 SELECT count(nif) INTO poblacion FROM personalpdi WHERE codigo IN 
  (SELECT codigo FROM todasestancias WHERE codigo IN (SELECT codigo FROM personalpdi)
                                           AND actividad = $1
                                           AND substring(codigo from 1 for 2) = $2);
 IF poblacion > 0 THEN
  densidad = superficie/poblacion;
 END IF;
 RETURN densidad;
END;
$_$;

COMMENT ON FUNCTION quest_zona_densidadpdi(integer, character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado pdi en las estancias de una determinada actividad SIGUANET para un campus o sede. No computa estancias desocupadas.
Se ejecuta de dos formas:
- select * from quest_zona_densidadpdi(4, ''00'');
- select quest_zona_densidadpdi(8, ''00'');';

CREATE FUNCTION quest_zona_densidadpdi(tipo character varying, denominacion character varying, zona character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$DECLARE
 superficie float8;
 poblacion int8;
 densidad float8 := 0;
 validacion integer;
 tipo_ varchar;
 actlist integer[];
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT ARRAY(SELECT codactividad FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ');'
   INTO actlist;
  SELECT quest_zona_superficieestanciasocupadaspdi(actlist, zona) INTO superficie;
  EXECUTE 'SELECT count(nif) FROM personalpdi WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 2) = ' || quote_literal(zona) || ');'
   INTO poblacion;
  IF poblacion > 0 THEN
   densidad = superficie/poblacion;
  END IF;
  RETURN densidad;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_densidadpdi(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene los m2 de espacio de trabajo por empleado tipo PDI en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un campus o sede. No computa estancias desocupadas.
SINTAXIS:
- SELECT quest_zona_densidadpdi(''crue'', ''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_numadmonnoocupados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND substring(codigo from 1 for 2) = upper($1);
$_$;

CREATE FUNCTION quest_zona_numadmonnoocupados(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    zona varchar;
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 2) = zona;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_zona_numadmonnoocupados(character varying, character varying) IS 'Obtiene el nº de administraciones no ocupadas de un departamento SIGUANET en un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_zona_numadmonnoocupados(''B101'', ''00'');
- select quest_zona_numadmonnoocupados(''B101'', ''00'');';

CREATE FUNCTION quest_zona_numadmonocupados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND substring(codigo from 1 for 2) = upper($1);
$_$;

CREATE FUNCTION quest_zona_numadmonocupados(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    zona varchar;
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 2) = zona;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_zona_numadmonocupados(character varying, character varying) IS 'Obtiene el nº de administraciones ocupadas de un departamento SIGUANET en un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_zona_numadmonocupados(''B101'', ''00'');
- select quest_zona_numadmonocupados(''B101'', ''00'');';

CREATE FUNCTION quest_zona_numbecarios(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM becarios WHERE substring(codigo from 1 for 2) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_zona_numbecarios(character varying) IS 'Obtiene el nº de becarios en un campus/sede de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_zona_numbecarios(''00'');
- select quest_zona_numbecarios(''00'');
';

CREATE FUNCTION quest_zona_numbecarios(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM becarios
	WHERE cod_depto_centro_subunidad = upper($1)
	AND substring(codigo FROM 1 FOR 2) = upper($2)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_zona_numbecarios(character varying, character varying) IS 'Obtiene el total de becarios de un departamento SIGUANET en un campus/sede de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_zona_numbecarios(''B101'', ''00'');
- select quest_zona_numbecarios(''B101'', ''00'');';

CREATE FUNCTION quest_zona_numbecarios(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM becarios WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 2) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_zona_numbecarios(tipo character varying, denominacion character varying, zona character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM becarios WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 2) = ' || quote_literal(zona) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_numbecarios(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene el nº de BECARIOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un campus o sede.
SINTAXIS:
- SELECT quest_zona_numbecarios(''crue'', ''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_numdespachosnoocupados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND substring(codigo from 1 for 2) = upper($1);
$_$;

CREATE FUNCTION quest_zona_numdespachosnoocupados(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    zona varchar;
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 2) = zona;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_zona_numdespachosnoocupados(character varying, character varying) IS 'Obtiene el nº de despachos no ocupados de un departamento SIGUANET en un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_zona_numdespachosnoocupados(''B101'', ''00'');
- select quest_zona_numdespachosnoocupados(''B101'', ''00'');';

CREATE FUNCTION quest_zona_numdespachosocupados(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND substring(codigo from 1 for 2) = upper($1);
$_$;

CREATE FUNCTION quest_zona_numdespachosocupados(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    zona varchar;
    cuenta int8;
    
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT codigo) INTO cuenta  
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 2) = zona;

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_zona_numdespachosocupados(character varying, character varying) IS 'Obtiene el nº de despachos ocupados de un departamento SIGUANET en un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_zona_numdespachosocupados(''B101'', ''00'');
- select quest_zona_numdespachosocupados(''B101'', ''00'');';

CREATE FUNCTION quest_zona_numestancias(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(codigo) FROM 
	(SELECT codigo from todasestancias where substring(codigo from 1 for 2) = upper($1) group by codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_zona_numestancias(character varying) IS 'Obtiene el nº de estancias en un campus/sede de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_zona_numestancias(''00'') ;
- select quest_zona_numestancias(''00'');
';

CREATE FUNCTION quest_zona_numestancias(integer, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    cuenta int8;

BEGIN
   IF $1 NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', $1;
   ELSE
	SELECT count(codigo) INTO cuenta FROM 
       (SELECT codigo from todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 2) = upper($2) GROUP BY codigo) AS foo;
      RETURN cuenta;
   END IF;
   
END
$_$;

COMMENT ON FUNCTION quest_zona_numestancias(integer, character varying) IS 'Obtiene el nº de estancias de una determinada actividad SIGUANET en un campus/sede de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_zona_numestancias(7, ''00'');
- select quest_zona_numestancias(8, ''00'');';

CREATE FUNCTION quest_zona_numestancias(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$DECLARE
    cuenta int8;
    adscripcion varchar;
    zona varchar;
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

SELECT count(codigo) INTO cuenta FROM 
(SELECT t.codigo FROM todasestancias t, departamentossigua d 
WHERE t.coddpto = d.cod_dpto_sigua
AND t.coddpto = adscripcion 
AND substring(t.codigo from 1 for 2) = zona
GROUP BY t.codigo) AS foo;

RETURN cuenta;

END;
$_$;

COMMENT ON FUNCTION quest_zona_numestancias(character varying, character varying) IS 'Obtiene el nº de estancias de un departamento SIGUANET en un campus/sede de la Universidad
SINTAXIS:
- SELECT quest_zona_numestancias(''B101'', ''00'');';

CREATE FUNCTION quest_zona_numestancias(integer, character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
   uso int4;
   adscripcion varchar;
   zona varchar;
   cuenta int8;
	
BEGIN
uso := $1;
adscripcion := upper($2);
zona := upper($3);
-- Control de errores
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;


   SELECT count(DISTINCT codigo) INTO cuenta FROM 
   todasestancias
   WHERE actividad = uso
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 2) =  zona;

   RETURN cuenta;
END;
$_$;

CREATE FUNCTION quest_zona_numestancias(tipo character varying, denominacion character varying, zona character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
 cuenta bigint;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT count(codigo)  FROM 
           (SELECT t.codigo FROM todasestancias t, actividades a 
             WHERE substring(t.codigo from 1 for 2) = ' || quote_literal(zona) ||
             ' AND t.actividad = a.codactividad
               AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ' GROUP BY t.codigo) AS foo;' 
  INTO cuenta;
 RETURN cuenta;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_numestancias(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene el nº de estancias de un tipo de grupo de actividad (crue, u21,activresum) y una denominación de grupo de actividad en un campus o sede.
SINTAXIS
SELECT quest_zona_numestancias(''crue'',''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_numestancias(character varying, character varying, character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
    agrupa varchar;
    denoactividad varchar;
    adscripcion varchar;
    zona varchar;
    cuenta int8;
	
BEGIN
agrupa := lower($1);
adscripcion := upper($3);
zona := upper($4);

IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
   RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
END IF;

IF agrupa = 'crue' THEN
	denoactividad := upper($2);
        IF denoactividad NOT IN ( SELECT crue FROM actividades) THEN
	   RAISE EXCEPTION 'La actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
		SELECT count(codigo) INTO cuenta FROM 
		(SELECT t.codigo FROM todasestancias t, departamentossigua d, actividades a 
		WHERE t.coddpto = d.cod_dpto_sigua
		AND t.actividad = a.codactividad
		AND t.coddpto = adscripcion 
		AND a.crue = denoactividad
		AND substring(t.codigo from 1 for 2) = zona
		GROUP BY t.codigo) AS foo;
		RETURN cuenta;
ELSEIF agrupa = 'u21' THEN
	denoactividad := upper($2);
        IF denoactividad NOT IN ( SELECT u21 FROM actividades) THEN
	   RAISE EXCEPTION 'La actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
		SELECT count(codigo) INTO cuenta FROM 
		(SELECT t.codigo FROM todasestancias t, departamentossigua d, actividades a 
		WHERE t.coddpto = d.cod_dpto_sigua
		AND t.actividad = a.codactividad
		AND t.coddpto = adscripcion 
		AND a.u21 = denoactividad
		AND substring(t.codigo from 1 for 2) = zona
		GROUP BY t.codigo) AS foo;
		RETURN cuenta;
ELSEIF agrupa = 'activresum' THEN
	denoactividad := $2;
        IF denoactividad NOT IN ( SELECT activresum FROM actividades) THEN
	   RAISE EXCEPTION 'La actividad % no pertenece a %.', denoactividad, agrupa;
        END IF;
		SELECT count(codigo) INTO cuenta FROM 
		(SELECT t.codigo FROM todasestancias t, departamentossigua d, actividades a 
		WHERE t.coddpto = d.cod_dpto_sigua
		AND t.actividad = a.codactividad
		AND t.coddpto = adscripcion 
		AND a.activresum = denoactividad
		AND substring(t.codigo from 1 for 2) = zona
		GROUP BY t.codigo) AS foo;
		RETURN cuenta;
ELSE
     RAISE exception 'No existe un campo valido con ese nombre: (%). Debe de ser u21, activresum o crue ', $1;
END IF;

END;

$_$;

CREATE FUNCTION quest_zona_numestanciasdocentes(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(codigo) FROM 
	(SELECT t.codigo from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE upper(a.activresum) = 'DOCENCIA'
         AND substring(codigo from 1 for 2) = $1
         group by t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_zona_numestanciasdocentes(character varying) IS 'Obtiene el nº de estancias de un campus/sede de la Universidad cuya actividad pertenece al grupo "Docencia" de la tabla actividades. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_zona_numestanciasdocentes(''00'');
- select quest_zona_numestanciasdocentes(''00'');';

CREATE FUNCTION quest_zona_numestanciasdocentes(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(codigo) FROM 
	(SELECT t.codigo from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE upper(a.activresum) = 'DOCENCIA'
	 AND t.coddpto = upper($1)
         AND substring(t.codigo from 1 for 2) = $2
         group by t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_zona_numestanciasdocentes(character varying, character varying) IS 'Obtiene el nº de estancias adscritas a un departamento SIGUANET en un campus/sede de la Universidad cuya actividad pertenece al grupo "Docencia" de la tabla actividades. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_zona_numestanciasdocentes(''B101'', ''00'');
- select quest_zona_numestanciasdocentes(''B101'', ''00'');';

CREATE FUNCTION quest_zona_numestanciasnoocupadas(integer, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4;
  zona varchar;
  cuenta int8;
BEGIN
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;

        uso := $1;
        zona := upper($2);
	SELECT count(DISTINCT codigo) INTO cuenta
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = uso
        AND substring(codigo from 1 for 2) = zona;

	RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_zona_numestanciasnoocupadas(integer, character varying) IS 'Obtiene el nº de estancias no ocupadas de una determinada actividad SIGUANET en un campus/sede de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_zona_numestanciasnoocupadas(50, ''00'');
- select quest_zona_numestanciasnoocupadas(50, ''00'');';

CREATE FUNCTION quest_zona_numestanciasnoocupadas(tipo character varying, denominacion character varying, zona character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
 cuenta bigint;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT count(DISTINCT t.codigo) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo NOT IN (SELECT codigo FROM todaspersonas)
             AND substring(t.codigo from 1 for 2) = ' || quote_literal(zona) || ';' INTO cuenta;
 RETURN cuenta;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_numestanciasnoocupadas(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene el nº de estancias no ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un campus o sede.
SINTAXIS
SELECT quest_zona_numestanciasnoocupadas(''crue'',''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_numestanciasocupadas(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND substring(codigo from 1 for 2) = upper($1)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_zona_numestanciasocupadas(integer, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4;
  zona varchar;
  cuenta int8;
BEGIN
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;

        uso := $1;
        zona := upper($2);
	SELECT count(DISTINCT codigo) INTO cuenta
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = uso
        AND substring(codigo from 1 for 2) = zona;

	RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_zona_numestanciasocupadas(integer, character varying) IS 'Obtiene el nº de estancias ocupadas de una determinada actividad SIGUANET en un campus/sede de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_zona_numestanciasocupadas(50, ''00'');
- select quest_zona_numestanciasocupadas(50, ''00'');';

CREATE FUNCTION quest_zona_numestanciasocupadas(integer[], character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4[];
  zona varchar;
  cuenta int8;
BEGIN
        uso := $1;
        zona := upper($2);
	SELECT count(DISTINCT codigo) INTO cuenta
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = ANY (uso)
        AND substring(codigo from 1 for 2) = zona;

	RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_zona_numestanciasocupadas(integer[], character varying) IS 'Obtiene el nº de estancias ocupadas de una lista de actividades SIGUANET en un campus/sede de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_zona_numestanciasocupadas(ARRAY[1,2,3,5], ''00'');
- select quest_zona_numestanciasocupadas(ARRAY[1,2,3,5], ''00'');';

CREATE FUNCTION quest_zona_numestanciasocupadas(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT codigo)
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND coddpto = upper($1)
	AND substring(codigo from 1 for 2) = upper($2)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_zona_numestanciasocupadas(tipo character varying, denominacion character varying, zona character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
 cuenta bigint;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT count(DISTINCT t.codigo) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo IN (SELECT codigo FROM todaspersonas)
             AND substring(t.codigo from 1 for 2) = ' || quote_literal(zona) || ';' INTO cuenta;
 RETURN cuenta;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_numestanciasocupadas(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene el nº de estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un campus o sede.
SINTAXIS
SELECT quest_zona_numestanciasocupadas(''crue'',''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_numestanciasutiles(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(*) FROM 
	(SELECT count(t.codigo) from todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE a.util = true AND substring(codigo from 1 for 2) = upper($1) GROUP BY t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_zona_numestanciasutiles(character varying) IS 'Obtiene el nº de estancias útiles en un campus/sede de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_zona_numestanciasutiles(''00'');
- select quest_zona_numestanciasutiles(''00'');
';

CREATE FUNCTION quest_zona_numestanciasutiles(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(*) FROM 
	(SELECT count(t.codigo)
	 FROM todasestancias t 
	 JOIN actividades a ON t.actividad = a.codactividad 
         WHERE a.util = true 
	 AND t.coddpto = upper($1)
	 AND substring(t.codigo from 1 for 2) = upper($2) GROUP BY t.codigo) AS foo;
$_$;

COMMENT ON FUNCTION quest_zona_numestanciasutiles(character varying, character varying) IS 'Obtiene el nº de estancias útiles adscritas a un departamento SIGUANET en un campus/sede de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_zona_numestanciasutiles(''B101'', ''00'');
- select quest_zona_numestanciasutiles(''B101'', ''00'');';

CREATE FUNCTION quest_zona_numexternos(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalexternos WHERE substring(codigo from 1 for 2) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_zona_numexternos(character varying) IS 'Obtiene el nº de externos en un campus/sede de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_zona_numexternos(''00'');
- select quest_zona_numexternos(''00'');
';

CREATE FUNCTION quest_zona_numexternos(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalexternos
	WHERE cod_dpto_sigua = upper($1)
	AND substring(codigo FROM 1 FOR 2) = upper($2)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_zona_numexternos(character varying, character varying) IS 'Obtiene el total de externos de un departamento SIGUANET en un campus/sede de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_zona_numexternos(''B101'', ''00'');
- select quest_zona_numexternos(''B101'', ''00'');';

CREATE FUNCTION quest_zona_numexternos(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM personalexternos WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 2) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_zona_numexternos(tipo character varying, denominacion character varying, zona character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM personalexternos WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 2) = ' || quote_literal(zona) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_numexternos(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene el nº de empleados EXTERNOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un campus o sede.
SINTAXIS:
- SELECT quest_zona_numexternos(''crue'', ''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_numpas(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalpas WHERE substring(codigo from 1 for 2) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_zona_numpas(character varying) IS 'Obtiene el nº de PAS en un campus/sede de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_zona_numpas(''00'');
- select quest_zona_numpas(''00'');
';

CREATE FUNCTION quest_zona_numpas(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalpas
	WHERE cod_unidad = upper($1)
	AND substring(codigo FROM 1 FOR 2) = upper($2)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_zona_numpas(character varying, character varying) IS 'Obtiene el total de PAS de un departamento SIGUANET en un campus/sede de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_zona_numpas(''B101'', ''00'');
- select quest_zona_numpas(''B101'', ''00'');';

CREATE FUNCTION quest_zona_numpas(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM personalpas WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 2) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_zona_numpas(tipo character varying, denominacion character varying, zona character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM personalpas WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 2) = ' || quote_literal(zona) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_numpas(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene el nº de PAS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un campus o sede.
SINTAXIS:
- SELECT quest_zona_numpas(''crue'', ''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_numpdi(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalpdi WHERE substring(codigo from 1 for 2) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_zona_numpdi(character varying) IS 'Obtiene el nº de PDI en un campus/sede de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_zona_numpdi(''00'');
- select quest_zona_numpdi(''00'');
';

CREATE FUNCTION quest_zona_numpdi(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalpdi
	WHERE cod_depto = upper($1)
	AND substring(codigo FROM 1 FOR 2) = upper($2)
	AND codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_zona_numpdi(character varying, character varying) IS 'Obtiene el total de PDI de un departamento SIGUANET en un campus/sede de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_zona_numpdi(''B101'', ''00'');
- select quest_zona_numpdi(''B101'', ''00'');';

CREATE FUNCTION quest_zona_numpdi(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM personalpdi WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 2) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_zona_numpdi(tipo character varying, denominacion character varying, zona character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM personalpdi WHERE codigo IN
            (SELECT codigo FROM quest_estancias 
              WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) ||
          '   AND substring(codigo FROM 1 FOR 2) = ' || quote_literal(zona) || ');'
   INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_numpdi(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene el nº de PDI ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un campus o sede.
SINTAXIS:
- SELECT quest_zona_numpdi(''crue'', ''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_numpdicargos(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
   SELECT count(*) FROM (SELECT DISTINCT nif FROM personalpdi_cargos WHERE substring(codigo from 1 for 2) = upper($1) AND codigo != '0000PB997') aux;
$_$;

COMMENT ON FUNCTION quest_zona_numpdicargos(character varying) IS 'Obtiene el nº de PDI que desempeñan cargo en un campus/sede de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_zona_numpdicargos(''00'');
- select quest_zona_numpdicargos(''00'');
';

CREATE FUNCTION quest_zona_numpdicargos(character varying, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
	SELECT count(DISTINCT nif)
	FROM personalpdi_cargos p
	JOIN cargos_dpto cd ON p.cod_cargo = cd.cod_cargo
	WHERE cd.coddpto = upper($1)
	AND substring(p.codigo from 1 for 2) = upper($2) 
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_zona_numpdicargos(character varying, character varying) IS 'Obtiene el nº de PDI que desempeñan cargo en un departamento sigua y en un campus/sede de la Universidad. 
Se ejecuta de dos formas:
- select * from quest_zona_numpdicargos(''B101'', ''00'');
- select quest_zona_numpdicargos(''B101'', ''00'');';

CREATE FUNCTION quest_zona_numpersonas(character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$SELECT count (DISTINCT nif) FROM todaspersonas WHERE substring (codigo from 1 for 2) = $1 AND codigo != '0000PB997'$_$;

COMMENT ON FUNCTION quest_zona_numpersonas(character varying) IS 'Obtiene el nº de personas en un campus/sede de la Universidad.
Se ejecuta de dos formas:
- select * from quest_zona_numpersonas(''00'');
- select quest_zona_numpersonas(''00'');
';

CREATE FUNCTION quest_zona_numpersonas(character varying, character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $_$DECLARE
    cuenta int8;
    adscripcion varchar;
    zona varchar;
BEGIN
   adscripcion := upper($1);
   zona := $2;
   IF adscripcion NOT IN (SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT count(DISTINCT nif) INTO cuenta 
   FROM todaspersonas
   WHERE cod_depto = adscripcion
   AND substring(codigo FROM 1 FOR 2) = zona
   AND codigo != '0000PB997';

   RETURN cuenta;
END;
$_$;

COMMENT ON FUNCTION quest_zona_numpersonas(character varying, character varying) IS 'Obtiene el nº de personas de un departamento sigua en un campus/sede de la Universidad, indicando si es pas, pdi, becario, externo o alguna de sus combinaciones
SINTAXIS:
- SELECT quest_zona_numpersonas(''B101'', ''00'');';

CREATE FUNCTION quest_zona_numpersonas(integer, character varying) RETURNS bigint
    LANGUAGE sql
    AS $_$
  SELECT count(*) FROM
   (SELECT nif FROM todaspersonas WHERE codigo IN
    (SELECT codigo FROM todasestancias WHERE actividad = $1 AND substring(codigo from 1 for 2) = $2)
   GROUP BY 1) foo;
$_$;

CREATE FUNCTION quest_zona_numpersonas(tipo character varying, denominacion character varying, zona character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$DECLARE
 cuenta bigint;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  EXECUTE 'SELECT count(DISTINCT nif) FROM todaspersonas WHERE codigo IN
            (SELECT codigo FROM quest_estancias WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
            ' AND substring(codigo FROM 1 FOR 2) = ' || quote_literal(zona) || ');' INTO cuenta;
  RETURN cuenta;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_numpersonas(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene el nº de personas ubicadas en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad, en un campus o sede.
SINTAXIS:
- SELECT quest_zona_numpersonas(''crue'', ''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_obteneradmonnoocupados(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias
WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
AND actividad = 8
AND codzona = $1;
$_$;

CREATE FUNCTION quest_zona_obteneradmonnoocupados(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND coddpto = upper($1)
	AND codzona = $2;
$_$;

CREATE FUNCTION quest_zona_obteneradmonocupados(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias
WHERE codigo IN (SELECT codigo FROM todaspersonas)
AND actividad = 8
AND codzona = $1;
$_$;

CREATE FUNCTION quest_zona_obteneradmonocupados(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 8
	AND coddpto = upper($1)
	AND codzona = $2;
$_$;

CREATE FUNCTION quest_zona_obtenerdespachosnoocupados(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias
WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
AND actividad = 7
AND codzona = $1;
$_$;

CREATE FUNCTION quest_zona_obtenerdespachosnoocupados(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND coddpto = upper($1)
	AND codzona = $2;
$_$;

CREATE FUNCTION quest_zona_obtenerdespachosocupados(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
SELECT * FROM quest_estancias
WHERE codigo IN (SELECT codigo FROM todaspersonas)
AND actividad = 7
AND codzona = $1;
$_$;

CREATE FUNCTION quest_zona_obtenerdespachosocupados(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = 7
	AND coddpto = upper($1)
	AND codzona = $2;
$_$;

CREATE FUNCTION quest_zona_obteneredificios(character varying)
  RETURNS SETOF quest_edificio AS
$BODY$
	SELECT * FROM quest_edificios() WHERE zona = $1 ORDER BY codigo;
$BODY$
LANGUAGE sql VOLATILE;

CREATE FUNCTION quest_zona_obteneredificios(character varying, character varying) RETURNS SETOF public.edificios
    LANGUAGE sql
    AS $_$
	SELECT DISTINCT ed.* FROM edificios ed JOIN todasestancias e ON ed.cod_zona || ed.cod_edificio = substring(e.codigo FROM 1 FOR 4)
	WHERE ed.cod_zona = $1 AND e.coddpto = $2 
	ORDER BY ed.cod_edificio;
$_$;

CREATE FUNCTION quest_zona_obteneredificios(character varying, integer) RETURNS SETOF public.edificios
    LANGUAGE sql
    AS $_$
	SELECT ed.* FROM edificios ed JOIN 
         (SELECT substring(codigo FROM 1 FOR 4) AS edificio FROM todasestancias WHERE substring(codigo FROM 1 FOR 2) = $1 AND actividad = $2 GROUP BY 1) e 
         ON ed.cod_zona || ed.cod_edificio = e.edificio
	 ORDER BY ed.cod_edificio;
$_$;

CREATE FUNCTION quest_zona_obteneredificios(tipo character varying, denominacion character varying, zona character varying) RETURNS SETOF public.edificios
    LANGUAGE plpgsql
    AS $$
DECLARE
  validacion integer;
  c refcursor;
  edificio edificios%ROWTYPE;
  tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
        WHEN 'activresum' THEN 'denogrupo'
        WHEN 'crue' THEN 'denocrue'
        WHEN 'u21' THEN 'denou21'
        END;
  OPEN c FOR EXECUTE 'SELECT ed.* FROM edificios ed JOIN 
                       (SELECT substring(codigo FROM 1 FOR 4) AS edificio FROM quest_estancias 
                         WHERE substring(codigo FROM 1 FOR 2) = ' || quote_literal(zona) || 
                         ' AND lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || ' GROUP BY 1) e 
                       ON ed.cod_zona || ed.cod_edificio = e.edificio ORDER BY ed.cod_edificio;';
  LOOP
   FETCH c INTO edificio;
   EXIT WHEN NOT FOUND;
   RETURN NEXT edificio;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_obteneredificios(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene los edificios de un determinado campus o sede en los que existen estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS
SELECT * FROM quest_zona_obteneredificios(''crue'',''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_obtenerestancias(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila quest_estancias%ROWTYPE;
    adscripcion varchar;
    zona varchar;
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   FOR fila IN 
	SELECT * FROM quest_estancias 
	WHERE coddpto = adscripcion
	AND substring(codigo from 1 for 2) = zona
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_zona_obtenerestancias(character varying, character varying) IS 'Obtiene todas las estancias de un departamento sigua en un campus/sede de la Universidad
Ejemplo:
SELECT * FROM quest_zona_obtenerestancias(''B101'', ''00'');';

CREATE FUNCTION quest_zona_obtenerestancias(integer, character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila quest_estancias%ROWTYPE;
    act integer;
    zona varchar;
BEGIN
   act := $1;
   zona := upper($2);
   IF act NOT IN (SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION '% no es una actividad en la tabla actividades.', act;
   END IF;

   FOR fila IN 
	SELECT * FROM quest_estancias 
	WHERE actividad = act
	AND substring(codigo from 1 for 2) = zona
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_zona_obtenerestancias(integer, character varying) IS 'Obtiene todas las estancias designadas para un uso en un campus/sede de la Universidad
Ejemplo:
SELECT * FROM quest_zona_obtenerestancias(7, ''00'');';

CREATE FUNCTION quest_zona_obtenerestancias(tipo character varying, denominacion character varying, zona character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $$
DECLARE
  validacion integer;
  c refcursor;
  estancia quest_estancias%ROWTYPE;
  tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
        WHEN 'activresum' THEN 'denogrupo'
        WHEN 'crue' THEN 'denocrue'
        WHEN 'u21' THEN 'denou21'
        END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_estancias WHERE substring(codigo from 1 for 2) = ' || quote_literal(zona) || ' AND lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || ';';
  LOOP
   FETCH c INTO estancia;
   EXIT WHEN NOT FOUND;
   RETURN NEXT estancia;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_obtenerestancias(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene las estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un campus o sede.
SINTAXIS
SELECT quest_ua_obtenerestancias(''crue'',''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_obtenerestanciasdocentes(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias 
	WHERE upper(denogrupo) = 'DOCENCIA'
	AND codzona = $1;
$_$;

CREATE FUNCTION quest_zona_obtenerestanciasdocentes(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_estancias 
	WHERE upper(denogrupo) = 'DOCENCIA'
	AND coddpto = upper($1)
	AND codzona = $2;
$_$;

CREATE FUNCTION quest_zona_obtenerestanciasnoocupadas(integer, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1
	AND codzona = $2;

$_$;

CREATE FUNCTION quest_zona_obtenerestanciasnoocupadas(tipo character varying, denominacion character varying, zona character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $$
DECLARE
 validacion integer;
 c refcursor;
 estancia quest_estancias%ROWTYPE;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 SELECT INTO tipo_ CASE lower(tipo)
  WHEN 'activresum' THEN 'denogrupo'
  WHEN 'crue' THEN 'denocrue'
  WHEN 'u21' THEN 'denou21'
 END;
 OPEN c FOR EXECUTE 'SELECT * FROM quest_estancias 
                      WHERE lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || 
                    ' AND codigo NOT IN (SELECT codigo FROM todaspersonas) 
                      AND substring(codigo from 1 for 2) = ' || quote_literal(zona) || ';' ;
 LOOP
  FETCH c INTO estancia;
  EXIT WHEN NOT FOUND;
  RETURN NEXT estancia;
 END LOOP;
 RETURN;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_obtenerestanciasnoocupadas(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene las estancias no ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un en un campus o sede.
SINTAXIS
SELECT * FROM quest_zona_obtenerestanciasnoocupadas(''crue'',''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_obtenerestanciasocupadas(character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias 
	WHERE (actividad <= 50 OR actividad = 99)
	AND codigo IN (SELECT codigo FROM todaspersonas)
	AND codzona = $1
	AND codigo != '0000PB997';

$_$;

CREATE FUNCTION quest_zona_obtenerestanciasocupadas(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias 
	WHERE (actividad <= 50 OR actividad = 99)
	AND codigo IN (SELECT codigo FROM todaspersonas)
	AND coddpto = upper($1)
	AND codzona = $2
	AND codigo != '0000PB997';

$_$;

CREATE FUNCTION quest_zona_obtenerestanciasocupadas(integer, character varying) RETURNS SETOF quest_estancias
    LANGUAGE sql
    AS $_$
	SELECT *
	FROM quest_estancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = $1
	AND codzona = $2;

$_$;

CREATE FUNCTION quest_zona_obtenerestanciasocupadas(tipo character varying, denominacion character varying, zona character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $$
DECLARE
 validacion integer;
 c refcursor;
 estancia quest_estancias%ROWTYPE;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 SELECT INTO tipo_ CASE lower(tipo)
  WHEN 'activresum' THEN 'denogrupo'
  WHEN 'crue' THEN 'denocrue'
  WHEN 'u21' THEN 'denou21'
 END;
 OPEN c FOR EXECUTE 'SELECT * FROM quest_estancias 
                      WHERE lower(' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) || 
                    ' AND codigo IN (SELECT codigo FROM todaspersonas) 
                      AND substring(codigo from 1 for 2) = ' || quote_literal(zona) || ';' ;
 LOOP
  FETCH c INTO estancia;
  EXIT WHEN NOT FOUND;
  RETURN NEXT estancia;
 END LOOP;
 RETURN;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_obtenerestanciasocupadas(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene las estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un en un campus o sede.
SINTAXIS
SELECT * FROM quest_zona_obtenerestanciasocupadas(''crue'',''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_obtenerestanciasutiles(character varying, character varying) RETURNS SETOF quest_estancias
    LANGUAGE plpgsql
    AS $_$
DECLARE
    fila quest_estancias%ROWTYPE;
    adscripcion varchar;
    zona varchar;	
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   FOR fila IN 
	SELECT * FROM quest_estancias t JOIN actividades a ON t.actividad = a.codactividad 
	WHERE a.util = true 
	AND t.coddpto = adscripcion
	AND substring(t.codigo from 1 for 2) = zona
   LOOP
      RETURN NEXT fila;
   END LOOP;
RETURN;
END;
$_$;

COMMENT ON FUNCTION quest_zona_obtenerestanciasutiles(character varying, character varying) IS 'Obtiene todas las estancias útiles de un departamento sigua en un campus/sede de la Universidad
Ejemplo:
SELECT * FROM quest_ua_obtenerestancias(''B101'', ''00'');';

CREATE FUNCTION quest_zona_obtenerplantas(text) RETURNS SETOF text
    LANGUAGE sql
    AS $_$
	SELECT planta FROM quest_plantaszona() WHERE zona = $1 ORDER BY indice;
$_$;

CREATE FUNCTION quest_zona_obtenerplantas(character varying, character varying) RETURNS SETOF character varying
    LANGUAGE sql
    AS $_$
	SELECT DISTINCT planta FROM
	(SELECT p.planta 
	FROM quest_plantaszona() p JOIN todasestancias e ON p.zona = substring(e.codigo FROM 1 FOR 2) AND p.planta = substring(e.codigo FROM 5 FOR 2)
	WHERE p.zona = $1 AND e.coddpto = $2
	ORDER BY indice) AS foo;
$_$;

CREATE FUNCTION quest_zona_obtenerplantas(character varying, integer) RETURNS SETOF character varying
    LANGUAGE sql
    AS $_$
	SELECT p.planta
	FROM quest_plantaszona() p JOIN todasestancias e ON p.zona = substring(e.codigo FROM 1 FOR 2) AND p.planta = substring(e.codigo FROM 5 FOR 2)
	WHERE p.zona = $1 AND e.actividad = $2
	GROUP BY p.planta, p.indice
	ORDER BY p.indice;

$_$;

CREATE FUNCTION quest_zona_obtenerplantas(tipo character varying, denominacion character varying, zona character varying) RETURNS SETOF text
    LANGUAGE plpgsql
    AS $$
DECLARE
  validacion integer;
  c refcursor;
  planta text;
  tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
        WHEN 'activresum' THEN 'denogrupo'
        WHEN 'crue' THEN 'denocrue'
        WHEN 'u21' THEN 'denou21'
        END;
  OPEN c FOR EXECUTE 'SELECT p.planta FROM quest_plantaszona() p 
                       JOIN quest_estancias e ON p.zona = substring(e.codigo FROM 1 FOR 2) AND p.planta = substring(e.codigo FROM 5 FOR 2)
	               WHERE p.zona = ' || quote_literal(zona) || ' AND lower(e.' || quote_ident(lower(tipo_)) || ') = ' || quote_literal(lower(denominacion)) ||
	               ' GROUP BY p.planta, p.indice
	                 ORDER BY p.indice;';
  LOOP
   FETCH c INTO planta;
   EXIT WHEN NOT FOUND;
   RETURN NEXT planta;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_obtenerplantas(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene las plantas de un campus o sede en las que existen estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad.
SINTAXIS
SELECT * FROM quest_zona_obtenerplantas(''crue'',''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_obtenerplantasedificio(character varying) RETURNS SETOF quest_plantaedificio
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_plantasedificio()() WHERE zona = $1 ORDER BY zona, edificio, indice;
$_$;

CREATE FUNCTION quest_zona_superficie(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
SELECT sum(st_area(geometria)) FROM todasestancias
WHERE substring(codigo from 1 for 2) = upper($1);
$_$;

COMMENT ON FUNCTION quest_zona_superficie(character varying) IS 'Obtiene la superficie total de un campus/sede de la Universidad.';

CREATE FUNCTION quest_zona_superficie(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria)) 
	FROM todasestancias
	WHERE coddpto = upper($1)
	AND substring(codigo from 1 for 2) = upper($2);
$_$;

COMMENT ON FUNCTION quest_zona_superficie(character varying, character varying) IS 'Obtiene la superficie total de las estancias de un departamento SIGUANET en un campus/sede de la Universidad.';

CREATE FUNCTION quest_zona_superficie(integer, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria)) 
	FROM todasestancias
	WHERE actividad = $1
	AND substring(codigo from 1 for 2) = upper($2);
$_$;

COMMENT ON FUNCTION quest_zona_superficie(integer, character varying) IS 'Obtiene la superficie total de las estancias designadas para un uso SIGUANET en un campus/sede de la Universidad.';

CREATE FUNCTION quest_zona_superficie(tipo character varying, denominacion character varying, zona character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
 superficie double precision;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT sum(st_area(geometria)) FROM 
           (SELECT t.geometria FROM todasestancias t, actividades a 
             WHERE substring(t.codigo from 1 for 2) = ' || quote_literal(zona) ||
             ' AND t.actividad = a.codactividad
               AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || ') AS foo;'
  INTO superficie;
 RETURN superficie;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_superficie(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene la superficie que ocupan las estancias de un tipo de grupo de actividad (crue, u21,activresum) y una denominación de grupo de actividad en un campus o sede.
SINTAXIS
SELECT quest_zona_superficie(''crue'',''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_superficieadmonnoocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    zona varchar;
BEGIN
   zona := upper($1);
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND substring(codigo from 1 for 2) = zona;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_zona_superficieadmonnoocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    zona varchar;
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 2) = zona;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_zona_superficieadmonnoocupados(character varying, character varying) IS 'Obtiene la superficie de administraciones no ocupadas de un departamento SIGUANET en un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_zona_superficieadmonnoocupados(''B101'', ''00'');
- select quest_zona_superficieadmonnoocupados(''B101'', ''00'');';

CREATE FUNCTION quest_zona_superficieadmonocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    zona varchar;
BEGIN
   zona := upper($1);
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND substring(codigo from 1 for 2) = zona;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_zona_superficieadmonocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    zona varchar;
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 8
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 2) = zona;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_zona_superficieadmonocupados(character varying, character varying) IS 'Obtiene la superficie de administraciones ocupadas de un departamento SIGUANET en un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_zona_superficieadmonocupados(''B101'', ''00'');
- select quest_zona_superficieadmonocupados(''B101'', ''00'');';

CREATE FUNCTION quest_zona_superficiedespachosnoocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    zona varchar;
BEGIN
   zona := upper($1);
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND substring(codigo from 1 for 2) = zona;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_zona_superficiedespachosnoocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    zona varchar;
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 2) = zona;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_zona_superficiedespachosnoocupados(character varying, character varying) IS 'Obtiene la superficie de despachos no ocupados de un departamento SIGUANET en un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_zona_superficiedespachosnoocupados(''B101'', ''00'');
- select quest_zona_superficiedespachosnoocupados(''B101'', ''00'');';

CREATE FUNCTION quest_zona_superficiedespachosocupados(character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    sumag float8;
    zona varchar;
BEGIN
   zona := upper($1);
   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND substring(codigo from 1 for 2) = zona;

   RETURN sumag;

END;
$_$;

CREATE FUNCTION quest_zona_superficiedespachosocupados(character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
    adscripcion varchar;
    zona varchar;
    sumag float8;
    
BEGIN
   adscripcion := upper($1);
   zona := upper($2);
   IF adscripcion NOT IN ( SELECT cod_dpto_sigua FROM departamentossigua) THEN
      RAISE EXCEPTION '% no es una unidad, departamento o centro definida en la tabla departamentossigua.', adscripcion;
   END IF;

   SELECT sum(st_area(geometria)) INTO sumag
   FROM todasestancias 
   WHERE codigo IN (SELECT codigo FROM todaspersonas)
   AND actividad = 7
   AND coddpto = adscripcion
   AND substring(codigo from 1 for 2) = zona;

   RETURN sumag;
END;
$_$;

COMMENT ON FUNCTION quest_zona_superficiedespachosocupados(character varying, character varying) IS 'Obtiene la superficie de despachos ocupados de un departamento SIGUANET en un campus/sede de la Universidad. 
SINTAXIS:
- select * from quest_zona_superficiedespachosocupados(''B101'', ''00'');
- select quest_zona_superficiedespachosocupados(''B101'', ''00'');';

CREATE FUNCTION quest_zona_superficiedocente(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE upper(a.activresum) = 'DOCENCIA'
        AND substring(codigo from 1 for 2) = $1;
$_$;

COMMENT ON FUNCTION quest_zona_superficiedocente(character varying) IS 'Obtiene la superficie total de las estancias docentes de un campus/sede de la Universidad.
Se ejecuta de dos formas:
- select * from quest_zona_superficiedocente(''00'');
- select quest_zona_superficiedocente(''00'');';

CREATE FUNCTION quest_zona_superficiedocente(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE upper(a.activresum) = 'DOCENCIA'
	AND t.coddpto = upper($1)
        AND substring(t.codigo from 1 for 2) = $2;
$_$;

COMMENT ON FUNCTION quest_zona_superficiedocente(character varying, character varying) IS 'Obtiene la superficie total de las estancias docentes de una departamento SIGUANET en un campus/sede de la Universidad.
Se ejecuta de dos formas:
- select * from quest_zona_superficiedocente(''B101'', ''00'');
- select quest_zona_superficiedocente(''B101'', ''00'');';

CREATE FUNCTION quest_zona_superficieestanciasnoocupadas(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4;
  zona varchar;
  superficie float8;
BEGIN
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;

        uso := $1;
        zona := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo NOT IN (SELECT codigo FROM todaspersonas)
	AND actividad = uso
        AND substring(codigo from 1 for 2) = zona;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_zona_superficieestanciasnoocupadas(integer, character varying) IS 'Obtiene la superficie de las estancias no ocupadas de una determinada actividad SIGUANET en un campus/sede de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_zona_superficieestanciasnoocupadas(50, ''00'');
- select quest_zona_superficieestanciasnoocupadas(50, ''00'');';

CREATE FUNCTION quest_zona_superficieestanciasnoocupadas(tipo character varying, denominacion character varying, zona character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
 superficie double precision;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT sum(st_area(t.geometria)) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo NOT IN (SELECT codigo FROM todaspersonas) 
             AND substring(t.codigo from 1 for 2) = ' || quote_literal(zona) || ';' INTO superficie;
 RETURN superficie;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_superficieestanciasnoocupadas(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene la superficie de las estancias no ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un en un campus o sede.
SINTAXIS
SELECT quest_zona_superficieestanciasnoocupadas(''crue'',''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_superficieestanciasocupadas(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND substring(codigo from 1 for 2) = upper($1)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_zona_superficieestanciasocupadas(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4;
  zona varchar;
  superficie float8;
BEGIN
   IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
      RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
   END IF;

        uso := $1;
        zona := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = uso
        AND substring(codigo from 1 for 2) = zona;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_zona_superficieestanciasocupadas(integer, character varying) IS 'Obtiene la superficie de las estancias ocupadas de una determinada actividad SIGUANET en un campus/sede de la Universidad. Si hay varias estancias con el mismo código las cuenta como una sola.
Se ejecuta de dos formas:
- select * from quest_zona_superficieestanciasocupadas(50, ''00'');
- select quest_zona_superficieestanciasocupadas(50, ''00'');';

CREATE FUNCTION quest_zona_superficieestanciasocupadas(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4[];
  zona varchar;
  superficie float8;
BEGIN
        uso := $1;
        zona := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = ANY (uso)
        AND substring(codigo from 1 for 2) = zona;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_zona_superficieestanciasocupadas(integer[], character varying) IS 'Obtiene la superficie de las estancias ocupadas de una lista de actividades SIGUANET en un campus/sede de la Universidad.
Se ejecuta de dos formas:
- select * from quest_zona_superficieestanciasocupadas(ARRAY[1,2,3,5], ''00'');
- select quest_zona_superficieestanciasocupadas(ARRAY[1,2,3,5], ''00'');';

CREATE FUNCTION quest_zona_superficieestanciasocupadas(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND coddpto = upper($1)
	AND substring(codigo from 1 for 2) = upper($2)
	AND codigo != '0000PB997';
$_$;

CREATE FUNCTION quest_zona_superficieestanciasocupadas(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
  uso int4[];
  zona varchar;
  adscripcion varchar;
  superficie float8;
BEGIN
        uso := $1;
	adscripcion := upper($2);
        zona := upper($3);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM todaspersonas)
	AND actividad = ANY (uso)
	AND coddpto = adscripcion
        AND substring(codigo from 1 for 2) = zona;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_zona_superficieestanciasocupadas(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias ocupadas y adscritas a un departamento SIGUANET de una lista de actividades SIGUANET en un campus/sede de la Universidad.
Se ejecuta de dos formas:
- select * from quest_zona_superficieestanciasocupadas(ARRAY[1,2,3,5], ''B101'', ''00'');
- select quest_zona_superficieestanciasocupadas(ARRAY[1,2,3,5], ''B101'', ''00'');';

CREATE FUNCTION quest_zona_superficieestanciasocupadas(tipo character varying, denominacion character varying, zona character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
DECLARE
 superficie double precision;
 validacion integer;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
IF validacion = 0 THEN
 EXECUTE 'SELECT sum(st_area(t.geometria)) FROM todasestancias t, actividades a 
             WHERE t.actividad = a.codactividad
             AND lower(a.' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) || 
           ' AND t.codigo IN (SELECT codigo FROM todaspersonas) 
             AND substring(t.codigo from 1 for 2) = ' || quote_literal(zona) || ';' INTO superficie;
 RETURN superficie;
ELSEIF validacion = 1 THEN
 RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
ELSEIF validacion = 2 THEN
 RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
ELSE
 RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_superficieestanciasocupadas(tipo character varying, denominacion character varying, zona character varying) IS 'Obtiene la superficie de las estancias ocupadas de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un en un campus o sede.
SINTAXIS
SELECT quest_zona_superficieestanciasocupadas(''crue'',''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_superficieestanciasocupadasbec(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	zona varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        zona := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM becarios)
	AND actividad = $1
	AND substring(codigo from 1 for 2) = zona;
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_zona_superficieestanciasocupadasbec(integer, character varying) IS 'Obtiene la superficie de las estancias de un campus/sede ocupadas por becarios de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_zona_superficieestanciasocupadasbecarios(8, ''00'');
- SELECT quest_zona_superficieestanciasocupadasbecarios(8, ''00'');
';

CREATE FUNCTION quest_zona_superficieestanciasocupadasbec(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	zona varchar;
	superficie float8;
BEGIN
        uso := $1;
        zona := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM becarios)
	AND actividad = ANY ($1)
	AND substring(codigo from 1 for 2) = zona;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_zona_superficieestanciasocupadasbec(integer[], character varying) IS 'Obtiene la superficie de las estancias de un campus/sede ocupadas por becarios de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_zona_superficieestanciasocupadasbecarios(ARRAY[8,9,16],''00'');
- SELECT quest_zona_superficieestanciasocupadasbecarios(ARRAY[8,9,16],''00'');
';

CREATE FUNCTION quest_zona_superficieestanciasocupadasbec(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM becarios)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 1 for 2) = upper($3);
$_$;

COMMENT ON FUNCTION quest_zona_superficieestanciasocupadasbec(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por becarios de una lista de actividades SIGUANET y en un campus/sede de la Universidad. 
SINTAXIS:
- SELECT * FROM quest_zona_superficieestanciasocupadasbecarios(ARRAY[8,9,16], ''B101'', ''00'');
- SELECT quest_zona_superficieestanciasocupadasbecarios(ARRAY[8,9,16], ''B101'', ''00'');';

CREATE FUNCTION quest_zona_superficieestanciasocupadasext(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	zona varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        zona := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalexternos)
	AND actividad = $1
	AND substring(codigo from 1 for 2) = zona;
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_zona_superficieestanciasocupadasext(integer, character varying) IS 'Obtiene la superficie de las estancias de un campus/sede ocupadas por externos de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_zona_superficieestanciasocupadasexternos(8, ''00'');
- SELECT quest_zona_superficieestanciasocupadasexternos(8, ''00'');
';

CREATE FUNCTION quest_zona_superficieestanciasocupadasext(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	zona varchar;
	superficie float8;
BEGIN
        uso := $1;
        zona := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalexternos)
	AND actividad = ANY ($1)
	AND substring(codigo from 1 for 2) = zona;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_zona_superficieestanciasocupadasext(integer[], character varying) IS 'Obtiene la superficie de las estancias de un campus/sede ocupadas por externos de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_zona_superficieestanciasocupadasexternos(ARRAY[8,9,16],''00'');
- SELECT quest_zona_superficieestanciasocupadasexternos(ARRAY[8,9,16],''00'');
';

CREATE FUNCTION quest_zona_superficieestanciasocupadasext(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalexternos)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 1 for 2) = upper($3);
$_$;

COMMENT ON FUNCTION quest_zona_superficieestanciasocupadasext(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por externos de una lista de actividades SIGUANET y en un campus/sede de la Universidad. 
SINTAXIS:
- SELECT * FROM quest_zona_superficieestanciasocupadasext(ARRAY[8,9,16], ''B101'', ''00'');
- SELECT quest_zona_superficieestanciasocupadasext(ARRAY[8,9,16], ''B101'', ''00'');';

CREATE FUNCTION quest_zona_superficieestanciasocupadaspas(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	zona varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        zona := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpas)
	AND actividad = $1
	AND substring(codigo from 1 for 2) = zona;
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_zona_superficieestanciasocupadaspas(integer, character varying) IS 'Obtiene la superficie de las estancias de un campus/sede ocupadas por PAS de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_zona_superficieestanciasocupadaspas(8, ''00'');
- SELECT quest_zona_superficieestanciasocupadaspas(8, ''00'');
';

CREATE FUNCTION quest_zona_superficieestanciasocupadaspas(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	zona varchar;
	superficie float8;
BEGIN
        uso := $1;
        zona := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpas)
	AND actividad = ANY ($1)
	AND substring(codigo from 1 for 2) = zona;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_zona_superficieestanciasocupadaspas(integer[], character varying) IS 'Obtiene la superficie de las estancias de un campus/sede ocupadas por PAS de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_zona_superficieestanciasocupadaspas(ARRAY[8,9,16],''00'');
- SELECT quest_zona_superficieestanciasocupadaspas(ARRAY[8,9,16],''00'');
';

CREATE FUNCTION quest_zona_superficieestanciasocupadaspas(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpas)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 1 for 2) = upper($3);
$_$;

COMMENT ON FUNCTION quest_zona_superficieestanciasocupadaspas(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por PAS de una lista de actividades SIGUANET y en un campus/sede de la Universidad. 
SINTAXIS:
- SELECT * FROM quest_zona_superficieestanciasocupadaspas(ARRAY[8,9,16], ''B101'', ''0'');
- SELECT quest_zona_superficieestanciasocupadaspas(ARRAY[8,9,16], ''B101'', ''0'');';

CREATE FUNCTION quest_zona_superficieestanciasocupadaspdi(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4;
	zona varchar;
	superficie float8;
BEGIN
	IF uso NOT IN ( SELECT codactividad FROM actividades) THEN
	 RAISE EXCEPTION 'La actividad % no está definida en la tabla Actividades.', uso;
	END IF;

        uso := $1;
        zona := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpdi)
	AND actividad = $1
	AND substring(codigo from 1 for 2) = zona;
	
	RETURN superficie;

END;
$_$;

COMMENT ON FUNCTION quest_zona_superficieestanciasocupadaspdi(integer, character varying) IS 'Obtiene la superficie de las estancias de un campus/sede ocupadas por PDI de una determinada actividad SIGUANET 
SINTAXIS:
- SELECT * FROM quest_zona_superficieestanciasocupadaspdi(8, ''00'');
- SELECT quest_zona_superficieestanciasocupadaspdi(8, ''00'');
';

CREATE FUNCTION quest_zona_superficieestanciasocupadaspdi(integer[], character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
DECLARE
	uso int4[];
	zona varchar;
	superficie float8;
BEGIN
        uso := $1;
        zona := upper($2);
	SELECT sum(st_area(geometria)) INTO superficie
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpdi)
	AND actividad = ANY ($1)
	AND substring(codigo from 1 for 2) = zona;

	RETURN superficie;
END;
$_$;

COMMENT ON FUNCTION quest_zona_superficieestanciasocupadaspdi(integer[], character varying) IS 'Obtiene la superficie de las estancias de un campus/sede ocupadas por PDI de una lista de actividades SIGUANET 
SINTAXIS:
- SELECT * FROM quest_zona_superficieestanciasocupadaspdi(ARRAY[8,9,16],''00'');
- SELECT quest_zona_superficieestanciasocupadaspdi(ARRAY[8,9,16],''00'');
';

CREATE FUNCTION quest_zona_superficieestanciasocupadaspdi(integer[], character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(geometria))
	FROM todasestancias 
	WHERE codigo IN (SELECT codigo FROM personalpdi)
	AND actividad = ANY ($1)
	AND coddpto = upper($2)
	AND substring(codigo from 1 for 2) = upper($3);
$_$;

COMMENT ON FUNCTION quest_zona_superficieestanciasocupadaspdi(integer[], character varying, character varying) IS 'Obtiene la superficie de las estancias de un departamento SIGUANET ocupadas por PDI de una lista de actividades SIGUANET y en un campus/sede de la Universidad. 
SINTAXIS:
- SELECT * FROM quest_zona_superficieestanciasocupadaspdi(ARRAY[8,9,16], ''B101'', ''00'');
- SELECT quest_zona_superficieestanciasocupadaspdi(ARRAY[8,9,16], ''B101'', ''00'');';

CREATE FUNCTION quest_zona_superficieutil(character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
         WHERE a.util = true AND substring(t.codigo from 1 for 2) = upper($1);
$_$;

COMMENT ON FUNCTION quest_zona_superficieutil(character varying) IS 'Obtiene la superficie útil de un campus/sede de la Universidad.
Se ejecuta de dos formas:
- select * from quest_zona_superficieutil(''00'');
- select quest_zona_superficieutil(''00'');
';

CREATE FUNCTION quest_zona_superficieutil(character varying, character varying) RETURNS double precision
    LANGUAGE sql
    AS $_$
	SELECT sum(st_area(t.geometria)) 
	FROM todasestancias t JOIN actividades a ON t.actividad = a.codactividad 
        WHERE a.util = true 
	AND t.coddpto = upper($1)
	AND substring(t.codigo from 1 for 2) = upper($2);
$_$;

COMMENT ON FUNCTION quest_zona_superficieutil(character varying, character varying) IS 'Obtiene la superficie útil de un departamento SIGUANET en un campus/sede de la Universidad.
Se ejecuta de dos formas:
- select * from quest_zona_superficieutil(''B101'', ''00'');
- select quest_zona_superficieutil(''B101'', ''00'');';

CREATE FUNCTION quest_zona_ubicaciones(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE codzona = $1;
$_$;

COMMENT ON FUNCTION quest_zona_ubicaciones(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de un campus/sede de la Universidad.
SINTAXIS
- SELECT * FROM quest_zona_ubicaciones(''00'')';

CREATE FUNCTION quest_zona_ubicaciones(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT DISTINCT v.* FROM quest_ubicaciones v
	JOIN quest_personas2 p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE p.cod_depto = upper($1)
	AND v.codzona = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_zona_ubicaciones(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de las personas adscritas a un departamento SIGUANET en un campus/sede de la Universidad.
SINTAXIS
- SELECT * FROM quest_zona_ubicaciones(''B101'', ''00'')';

CREATE FUNCTION quest_zona_ubicaciones(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones WHERE actividad = $1 AND substring(codigo from 1 for 2) = $2;
$_$;

COMMENT ON FUNCTION quest_zona_ubicaciones(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en un campus o sede.
SINTAXIS
- SELECT * FROM quest_zona_ubicaciones(7, ''00'')';

CREATE FUNCTION quest_zona_ubicaciones(tipo character varying, denominacion character varying, zona character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND substring(codigo FROM 1 FOR 2) = ' || quote_literal(zona) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_ubicaciones(tipo character varying, denominacion character varying, zona character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para las personas ubicadas en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad, en un campus o sede.
SINTAXIS:
- SELECT * FROM quest_zona_ubicaciones(''crue'', ''DOCENCIA'', ''00'');';

CREATE FUNCTION quest_zona_ubicacionesbecarios(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones
   WHERE esbecario = true 
   AND locbecario = true
   AND reftbl = 'becarios'
   AND (actividad <= 50 OR actividad = 99)
   AND codzona = $1;
$_$;

COMMENT ON FUNCTION quest_zona_ubicacionesbecarios(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de un campus/sede de la Universidad.
La condición where matiza que sólo devuelve estancias con BECARIOS
SINTAXIS
- SELECT * FROM quest_zona_ubicacionesbecarios(''00'')';

CREATE FUNCTION quest_zona_ubicacionesbecarios(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_becarios p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.esbecario = true 
	AND v.locbecario = true
	AND v.reftbl = 'becarios'
	AND p.cod_depto_centro_subunidad = upper($1)
	AND v.codzona = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_zona_ubicacionesbecarios(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociación becario/estancia de los becarios adscritos a un departamento SIGUANET ubicados en un campus/sede de la Universidad.
SINTAXIS
- SELECT * FROM quest_zona_ubicacionesbecarios(''B101'', ''00'')';

CREATE FUNCTION quest_zona_ubicacionesbecarios(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 
   AND substring(codigo from 1 for 2) = $2 
   AND esbecario = true 
   AND locbecario = true
   AND reftbl = 'becarios';
$_$;

COMMENT ON FUNCTION quest_zona_ubicacionesbecarios(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion becario/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en un campus o sede.
SINTAXIS
- SELECT * FROM quest_zona_ubicacionesbecarios(7, ''00'')';

CREATE FUNCTION quest_zona_ubicacionesbecarios(tipo character varying, denominacion character varying, zona character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND esbecario = true 
                       AND locbecario = true
                       AND reftbl = ''becarios''
                       AND substring(codigo FROM 1 FOR 2) = ' || quote_literal(zona) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_ubicacionesbecarios(tipo character varying, denominacion character varying, zona character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los BECARIOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un campus o sede.
SINTAXIS:
- SELECT * FROM quest_zona_ubicacionesbecarios(''crue'', ''DOCENCIA'', 00'');';

CREATE FUNCTION quest_zona_ubicacionescargos(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE espdicargo = true 
   AND locpdicargo = true
   AND reftbl = 'personalpdi_cargos'
   AND (actividad <= 50 OR actividad = 99)
   AND codzona = $1;
$_$;

COMMENT ON FUNCTION quest_zona_ubicacionescargos(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de un campus/sede de la Universidad.
La condición where matiza que sólo devuelve estancias con CARGOS
SINTAXIS
- SELECT * FROM quest_zona_ubicacionescargos(''00'')';

CREATE FUNCTION quest_zona_ubicacionescargos(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalpdi_cargos p ON v.nif = p.nif AND v.codigo = p.codigo
	JOIN cargos_dpto cd ON p.cod_cargo = cd.cod_cargo
	WHERE v.espdicargo = true 
	AND v.locpdicargo = true
	AND v.reftbl = 'personalpdi_cargos'
	AND cd.coddpto = upper($1)
	AND v.codzona = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_zona_ubicacionescargos(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociación pdicargo/estancia de los cargos adscritos a un departamento SIGUANET ubicados en un campus/sede de la Universidad.
SINTAXIS
- SELECT * FROM quest_zona_ubicacionescargos(''B101'', ''00'')';

CREATE FUNCTION quest_zona_ubicacionesexternos(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones
   WHERE esexterno = true
   AND locexterno = true
   AND reftbl = 'personalexternos'
   AND (actividad <= 50 OR actividad = 99)
   AND codzona = $1;
$_$;

COMMENT ON FUNCTION quest_zona_ubicacionesexternos(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de un campus/sede de la Universidad.
La condición where matiza que sólo devuelve estancias DE EXTERNOS
SINTAXIS
- SELECT * FROM quest_zona_ubicacionesexternos(''00'')';

CREATE FUNCTION quest_zona_ubicacionesexternos(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalexternos p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.esexterno = true 
	AND v.locexterno = true
	AND v.reftbl = 'personalexternos'
	AND p.cod_dpto_sigua = upper($1)
	AND v.codzona = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_zona_ubicacionesexternos(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion externo/estancia de los externos adscritos a un departamento SIGUANET ubicados en un campus/sede de la Universidad.
SINTAXIS
- SELECT * FROM quest_zona_ubicacionesexternos(''B101'', ''00'')';

CREATE FUNCTION quest_zona_ubicacionesexternos(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 
   AND substring(codigo from 1 for 2) = $2 
   AND esexterno = true 
   AND locexterno = true
   AND reftbl = 'personalexternos';
$_$;

COMMENT ON FUNCTION quest_zona_ubicacionesexternos(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion externo/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en un campus o sede.
SINTAXIS
- SELECT * FROM quest_zona_ubicacionesexternos(7, ''00'')';

CREATE FUNCTION quest_zona_ubicacionesexternos(tipo character varying, denominacion character varying, zona character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND esexterno = true 
                       AND locexterno = true
                       AND reftbl = ''personalexternos''
                       AND substring(codigo FROM 1 FOR 2) = ' || quote_literal(zona) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_ubicacionesexternos(tipo character varying, denominacion character varying, zona character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los empleados EXTERNOS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un campus o sede.
SINTAXIS:
- SELECT * FROM quest_zona_ubicacionesexternos(''crue'', ''DOCENCIA'', 00'');';

CREATE FUNCTION quest_zona_ubicacionespas(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT * FROM quest_ubicaciones 
	WHERE espas = true
	AND locpas = true
	AND reftbl = 'personalpas'
	AND (actividad <= 50 OR actividad = 99)
	AND codzona = $1;
$_$;

COMMENT ON FUNCTION quest_zona_ubicacionespas(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de un campus/sede de la Universidad.
La condición where matiza que sólo devuelve estancias con pas
SINTAXIS
- SELECT * FROM quest_zona_ubicacionespas(''00'')';

CREATE FUNCTION quest_zona_ubicacionespas(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalpas p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.espas = true 
	AND v.locpas = true
	AND v.reftbl = 'personalpas'
	AND p.cod_unidad = upper($1)
	AND v.codzona = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_zona_ubicacionespas(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion pas/estancia de los pas adscritos a un departamento SIGUANET ubicados en un campus/sede de la Universidad.
La condición where matiza que sólo devuelve estancias con pas
SINTAXIS
- SELECT * FROM quest_zona_ubicacionespas(''B101'', ''00'')';

CREATE FUNCTION quest_zona_ubicacionespas(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 
   AND substring(codigo from 1 for 2) = $2 
   AND espas = true 
   AND locpas = true
   AND reftbl = 'personalpas';
$_$;

COMMENT ON FUNCTION quest_zona_ubicacionespas(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion PAS/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en un campus o sede.
SINTAXIS
- SELECT * FROM quest_zona_ubicacionespas(7, ''00'')';

CREATE FUNCTION quest_zona_ubicacionespas(tipo character varying, denominacion character varying, zona character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND espas = true 
                       AND locpas = true
                       AND reftbl = ''personalpas''
                       AND substring(codigo FROM 1 FOR 2) = ' || quote_literal(zona) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_ubicacionespas(tipo character varying, denominacion character varying, zona character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los PAS ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un campus o sede.
SINTAXIS:
- SELECT * FROM quest_zona_ubicacionespas(''crue'', ''DOCENCIA'', 00'');';

CREATE FUNCTION quest_zona_ubicacionespdi(character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE espdi = true 
   AND locpdi = true
   AND reftbl = 'personalpdi'
   AND (actividad <= 50 OR actividad = 99)
   AND codzona = $1;
$_$;

COMMENT ON FUNCTION quest_zona_ubicacionespdi(character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia de un campus/sede de la Universidad.
La condición where matiza que sólo devuelve estancias con PDI
SINTAXIS
- SELECT * FROM quest_zona_ubicacionespdi(''00'')';

CREATE FUNCTION quest_zona_ubicacionespdi(character varying, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
	SELECT v.* 
	FROM quest_ubicaciones v
	JOIN quest_personalpdi p ON v.nif = p.nif AND v.codigo = p.codigo
	WHERE v.espdi = true 
	AND v.locpdi = true
	AND v.reftbl = 'personalpdi'
	AND p.cod_depto = upper($1)
	AND v.codzona = upper($2)
	AND p.codigo != '0000PB997';
$_$;

COMMENT ON FUNCTION quest_zona_ubicacionespdi(character varying, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociación pdi/estancia de los pdi adscritos a un departamento SIGUANET ubicados en un campus/sede de la Universidad.
SINTAXIS
- SELECT * FROM quest_zona_ubicacionespdi(''B101'', ''00'')';

CREATE FUNCTION quest_zona_ubicacionespdi(integer, character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE sql
    AS $_$
   SELECT * FROM quest_ubicaciones 
   WHERE actividad = $1 
   AND substring(codigo from 1 for 2) = $2 
   AND espdi = true 
   AND locpdi = true
   AND reftbl = 'personalpdi';
$_$;

COMMENT ON FUNCTION quest_zona_ubicacionespdi(integer, character varying) IS 'Esta función utiliza una vista llamada
quest_ubicaciones, que devuelve todos los pares de asociacion pdi/estancia de la Universidad y devuelve los correspondientes a estancias de una determinada actividad SIGUANET en un campus o sede.
SINTAXIS
- SELECT * FROM quest_zona_ubicacionespdi(7, ''00'')';

CREATE FUNCTION quest_zona_ubicacionespdi(tipo character varying, denominacion character varying, zona character varying) RETURNS SETOF quest_ubicaciones
    LANGUAGE plpgsql
    AS $$DECLARE
 c refcursor;
 ubicacion quest_ubicaciones%ROWTYPE;
 validacion integer;
 tipo_ varchar;
BEGIN
 SELECT quest_validacion_grupoactividad(tipo, denominacion) INTO validacion;
 IF validacion = 0 THEN
  SELECT INTO tipo_ CASE lower(tipo)
                    WHEN 'activresum' THEN 'denogrupo'
                    WHEN 'crue' THEN 'denocrue'
                    WHEN 'u21' THEN 'denou21'
                    END;
  OPEN c FOR EXECUTE 'SELECT * FROM quest_ubicaciones WHERE lower(' || quote_ident(lower(tipo_))  || ') = ' || quote_literal(lower(denominacion)) || 
                     ' AND espdi = true 
                       AND locpdi = true
                       AND reftbl = ''personalpdi''
                       AND substring(codigo FROM 1 FOR 2) = ' || quote_literal(zona) || ';';
  LOOP
   FETCH c INTO ubicacion;
   EXIT WHEN NOT FOUND;
   RETURN NEXT ubicacion;
  END LOOP;
  RETURN;
 ELSEIF validacion = 1 THEN
  RAISE exception 'El parámetro de entrada % no corresponde a un tipo válido de grupo de actividad. Ejemplos de tipo válido son u21, activresum o crue ', tipo;
 ELSEIF validacion = 2 THEN
  RAISE EXCEPTION 'La denominación % no se corresponde con ninguno de los grupos de actividad de tipo %.', denominacion, tipo;
 ELSE
  RAISE EXCEPTION 'No se pudo realizar la validación del tipo % y del grupo de actividad %.', denominacion, tipo;
 END IF;
END;
$$;

COMMENT ON FUNCTION quest_zona_ubicacionespdi(tipo character varying, denominacion character varying, zona character varying) IS 'Esta función utiliza una vista llamada quest_ubicaciones, que devuelve todos los pares de asociacion persona/estancia para los PDI ubicados en estancias de un tipo de grupo de actividad (crue, u21, activresum) y una denominación de grupo de actividad en un campus o sede.
SINTAXIS:
- SELECT * FROM quest_zona_ubicacionespdi(''crue'', ''DOCENCIA'', 00'');';

CREATE FUNCTION quest_grupoactividad_obtenerdenomalt(denominacion character varying) RETURNS text
    LANGUAGE plpgsql
    AS $$
DECLARE
 denomalt text := '';
 array_denoms text[][] := ARRAY[
                                ['--', 'Usos no catalogados'],
                                ['AUDIOV', 'Aulas audiovisuales'],
                                ['AULA', 'Aulas de teoría'],
                                ['BIBLIO', 'Bibliotecas'],
                                ['DEPORTE', 'Instalaciones deportivas'],
                                ['DESP', 'Despachos'],
                                ['DIBUJO', 'Aulas de dibujo'],
                                ['INFLIBRE', 'Aulas de informática de libre acceso'],
                                ['INFORM', 'Aulas de informática'],
                                ['LABDOC', 'Laboratorios docentes'],
                                ['LABINV', 'Laboratorios de investigación'],
                                ['SALA', 'Salas de reunión'],
                                ['SALON', 'Salones de actos'],
                                ['SEM', 'Seminarios'],
                                ['SMUSOS', 'Salas multiuso']
                               ];
BEGIN
 FOR i IN 1..array_upper(array_denoms, 1) LOOP
  IF upper(array_denoms[i][1]) = upper(denominacion) THEN
   denomalt := array_denoms[i][2];
   EXIT;
  END IF;
 END LOOP;
 IF denomalt = '' THEN
  IF char_length(denominacion) > 1 THEN
   denomalt := upper(substring(denominacion from 1 for 1)) || lower(substring(denominacion from 2));
  ELSE
   denomalt := denominacion;
  END IF;
 END IF;
 RETURN denomalt;
END;$$;

COMMENT ON FUNCTION quest_grupoactividad_obtenerdenomalt(denominacion character varying) IS 'Función auxiliar que devuelve la denominación alternativa de un grupo de actividad.';

CREATE FUNCTION quest_validacion_grupoactividad(tipo character varying, denominacion character varying) RETURNS integer
    LANGUAGE plpgsql
    AS $$
DECLARE
 array_tipos varchar[] := ARRAY['activresum', 'crue', 'u21'];
 cuenta integer;
BEGIN
 IF lower(tipo) = ANY(array_tipos) THEN
  EXECUTE 'SELECT count(*) FROM actividades WHERE lower(' || quote_ident(lower(tipo)) || ') = ' || quote_literal(lower(denominacion)) INTO cuenta;
  IF cuenta > 0 THEN
   RETURN 0;
  ELSE
   RETURN 2;
  END IF;
 ELSE
   RETURN 1;
 END IF;
END;$$;

COMMENT ON FUNCTION quest_validacion_grupoactividad(tipo character varying, denominacion character varying) IS 'Función auxiliar para la validación de parámetros de tipo y denominación de grupo de actividad. Devuelve 0 si
el tipo y la denominación existen. Devuelve 1 si el tipo no existe, es decir, no coincide con ninguno de los nombres de campo definidos en la tabla de actividades para almacenar  grupos de actividad (i.e. activresum, crue, u21). Devuelve 2 si la denominación no coincide con ninguna de las posibles denominaciones de grupo almacenadas en la tabla de actividades para el tipo en cuestión (i.e. la denominación de grupo "servicios" no existe para el tipo de grupo activresum).
';

CREATE TABLE quest_adminroles
(
  grouprole text NOT NULL
);

COMMENT ON TABLE quest_adminroles IS 'Nombres de roles de grupo con privilegio de administrador a nivel de aplicación. Un usuario de quest con este privilegio puede recuperar el NIF (dato personal) sin encriptar. Los usuarios de PostgreSQL con privilegio de SUPERUSER cuentan con este privilegio por defecto, independientemente del rol de grupo al que pertenezcan.';


CREATE VIEW quest_estancias_label AS
    SELECT todasestancias.codigo, (public.st_pointonsurface(todasestancias.geometria)) AS pointonsurface FROM public.todasestancias WHERE (NOT (todasestancias.actividad = ANY (ARRAY[20, 80, 91, 92, 93, 96, 97, 98])));

COMMENT ON VIEW quest_estancias_label IS 'Vista de apoyo para generación de etiquetas de código SIGUANET';

CREATE VIEW quest_personas2 AS
    SELECT CASE WHEN quest_isadmin() THEN a.nif ELSE b.nifmd5 END AS nif, a.codigo, a.cod_depto FROM (public.todaspersonas a JOIN quest_nifmd5 b ON (((a.nif)::text = (b.nif)::text)));

COMMENT ON VIEW quest_personas2 IS 'Vista de la tabla todaspersonas empleada en joins necesarios para conteo.';

CREATE VIEW quest_listaedificios AS
    SELECT ((edificios.cod_zona)::text || (edificios.cod_edificio)::text) AS codze, edificios.txt_edificio FROM public.edificios ORDER BY edificios.cod_zona, edificios.cod_edificio;

COMMENT ON VIEW quest_listaedificios IS 'Devuelve un conjunto de registros con el código del edificio, en 4 dígitos y el  nombre del edificio';

ALTER TABLE ONLY quest_adminroles
    ADD CONSTRAINT quest_adminroles_pkey PRIMARY KEY (grouprole);

