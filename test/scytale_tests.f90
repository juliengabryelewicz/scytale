module scytale_tests
  use scytale, only: scytale_crypt, scytale_translate

  implicit none

  private
  public :: test_scytale_crypt, test_scytale_translate

contains

  subroutine test_scytale_crypt

    character(:), allocatable :: result,expect, sentence
    expect = "cstetec siut n e  "
    sentence = "ceci est un test"
    result = scytale_crypt(sentence)

    print *, 'test crypt with no index'
    print *, expect
    print *, result

    expect = "c  t eeue csns it t "
    result = scytale_crypt(sentence,5)


    print *, 'test crypt with index'
    print *, expect
    print *, result

  end subroutine test_scytale_crypt

  subroutine test_scytale_translate

    character(:), allocatable :: result,expect, sentence
    sentence = "cstetec siut n e  "
    expect = "ceci est un test"
    result = scytale_translate(sentence)

    print *, 'test translate with no index'
    print *, expect
    print *, result

    sentence = "c  t eeue csns it t "
    result = scytale_translate(sentence,5)

    print *, 'test translate with index'
    print *, expect
    print *, result

  end subroutine test_scytale_translate

end module scytale_tests


program run_tests
  use scytale_tests, only: test_scytale_crypt, test_scytale_translate
  call test_scytale_crypt()
  call test_scytale_translate()
end program run_tests