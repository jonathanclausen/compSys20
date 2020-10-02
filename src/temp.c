// result.load_store_runs = events->data_access_ok;
//     result.compute_runs = result.load_store_runs & !same(load_store->reg_d.out, compute->reg_d.out)
//                                                  & !same(load_store->reg_d.out, compute->reg_s.out)
//                                                  & !same(load_store->reg_d.out, compute->reg_z.out);
//     result.fetch_runs = result.compute_runs & events->insn_access_ok;


//     // Decide which instructions to keep/potentially pass on/drop

//     bool jmp = events->insn_flow_change_request;

//     result.fetch_valid = result.fetch_runs & !jmp;
//     compute->is_valid.in = result.fetch_valid;
//     load_store->is_valid.in = compute->is_valid.in;