subroutine irp_finalize_751899324
 use mat_mod
 use rank_mod
  if (allocated(m)) then
    m_is_built = .False.
    deallocate(m)
  endif
end
