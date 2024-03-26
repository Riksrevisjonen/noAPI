entity_resp <- get_entity(974760843)
roles_resp <- get_roles(974760843)
saveRDS(entity_resp, 'tests/testdata/brreg-entity-resp.rds')
saveRDS(roles_resp, 'tests/testdata/brreg-roles-resp.rds')
