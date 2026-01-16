BEGIN;
  ALTER TABLE $.attribute_type ADD COLUMN display_cost INTEGER;

  CREATE TRIGGER refresh_absolute_display_name
    AFTER INSERT OR UPDATE OR DELETE ON $.attribution_string
    EXECUTE PROCEDURE $.refresh_absolute_display_name();

  UPDATE $.global_integer SET global_value = 7
    WHERE global_name = 'schema_version';
END;
